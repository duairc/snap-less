{-# LANGUAGE OverloadedStrings #-}
module Snap.Less
    ( LessDirectory
    , newLessDirectory
    , newLessDirectory'
    , getLessState

    , lessHandler
    , lessReloadHandler

    , LessState
    , renderLess
    ) where
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import           Data.Either
import           Data.List
import qualified Data.Foldable as F
import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.Map as M
import           Snap.Types hiding (dir)
import           System.Directory
import           System.Directory.Tree
import           System.Exit
import           System.IO
import           System.Process

newtype LessState  = LessState (M.Map ByteString ByteString)
data LessDirectory = LessDirectory FilePath (MVar LessState)


newLessDirectory :: MonadIO m => FilePath -> m (Either String LessDirectory)
newLessDirectory dir = liftIO $ do
    els <- loadStylesheets dir
    leftPass els $ \ls -> do
        lsMVar <- newMVar ls
        return $ LessDirectory dir lsMVar


newLessDirectory' :: MonadIO m => FilePath -> m LessDirectory
newLessDirectory' = (either fail return =<<) . newLessDirectory


getLessState  :: MonadIO m => LessDirectory -> m LessState
getLessState (LessDirectory _ lsMVar) = liftIO $ readMVar lsMVar


loadStylesheet :: FilePath -> IO (Either String ByteString)
loadStylesheet file = do
    mLessc <- findLessc
    case mLessc of
        Nothing    -> return $ Left "Executable `lessc' could not be found"
        Just lessc -> do
            (ex, css, _) <- readProcessWithExitCode' lessc [file, stdout] ""
            return $ if (ex /= ExitSuccess) then Left file else Right css
  where
    stdout = "/dev/stdout"


loadStylesheets :: FilePath -> IO (Either String LessState)
loadStylesheets dir = do
    d <- readDirectoryWith f dir
    let slist  = justs $ F.toList (free d)
        errors = lefts slist
    return $ case errors of
        [] -> Right $ LessState $ M.fromList $ rights slist
        _  -> Left $ unlines errors
  where
    f p | ".less" `isSuffixOf` p = loadStylesheet p >>= return . Just . either Left (\t -> Right (cssify p, t))
        | otherwise              = return Nothing
    cssify p = B.pack $ (drop (length dir + 1) $ take (length p - 5) p) ++ ".css"
    justs xs = [a | Just a <- xs]


findLessc :: IO (Maybe FilePath)
findLessc = findExecutable "lessc" >>= maybe (findM exec gems) (return . Just)
  where
    gems = ["/var/lib/gems/1.9.1/bin/lessc", "/var/lib/gems/1.8/bin/lessc"]
    exec file = do
        exists <- doesFileExist file
        if exists then getPermissions file >>= return . executable
                  else return False


findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM f = liftM (lookup True) . mapM (\x -> f x >>= (\y -> return (y,x)))


lessHandler :: LessDirectory
            -> (LessDirectory -> Snap ())
            -> (LessState -> Snap ())
            -> Snap ()
lessHandler ld reload f = reload ld <|> (f =<< getLessState ld)



lessReloadHandler :: LessDirectory -> Snap ()
lessReloadHandler ld = path "admin/reload/less" $ do
    e <- reloadLessDirectory ld
    modifyResponse $ setContentType "text/plain; charset=utf-8"
    writeBS . B.pack $ either id (const "Stylesheets loaded successfully.\n") e


reloadLessDirectory :: MonadIO m => LessDirectory -> m (Either String ())
reloadLessDirectory (LessDirectory dir lsMVar) = liftIO $ do
    els <- loadStylesheets dir
    leftPass els $ \ls -> modifyMVar_ lsMVar (const $ return ls)


renderLess :: LessState -> Snap ()
renderLess (LessState m) = do
    file <- liftM rqPathInfo getRequest
    flip (maybe (writeBS $ B.pack $ show m)) (M.lookup file m) $ \css -> do
        modifyResponse $ setContentType "text/css; charset=utf-8"
        writeBS css


-- stolen from Heist
readProcessWithExitCode'
    :: FilePath
    -> [String]
    -> ByteString
    -> IO (ExitCode, ByteString, ByteString)
readProcessWithExitCode' cmd args input = do
    (Just inh, Just outh, Just errh, pid) <-
        createProcess (proc cmd args){ std_in  = CreatePipe,
                                       std_out = CreatePipe,
                                       std_err = CreatePipe }
    outMVar <- newEmptyMVar
    outM <- newEmptyMVar
    errM <- newEmptyMVar
    -- fork off a thread to start consuming stdout
    forkIO $ do
        out <- B.hGetContents outh
        putMVar outM out
        putMVar outMVar ()
    -- fork off a thread to start consuming stderr
    forkIO $ do
        err  <- B.hGetContents errh
        putMVar errM err
        putMVar outMVar ()
    -- now write and flush any input
    when (not (B.null input)) $ do B.hPutStr inh input; hFlush inh
    hClose inh -- done with stdin
    -- wait on the output
    takeMVar outMVar
    takeMVar outMVar
    hClose outh
    -- wait on the process
    ex <- waitForProcess pid
    out <- readMVar outM
    err <- readMVar errM
    return (ex, out, err)


leftPass :: Monad m => Either String b -> (b -> m c) -> m (Either String c)
leftPass e m = either (return . Left . loadError) (liftM Right . m) e
  where
    loadError = (++) "Error loading stylesheets: "
