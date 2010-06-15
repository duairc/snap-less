# snap-less: Less CSS for Snap

In your `Main.hs`, do this:

    main :: IO ()
    main = do
        ld <- newLessDirectory'     "stylesheets"
        td <- newTemplateDirectory' "templates"   $ withSplices splices
        quickServer $ lessHandler     ld lessReloadHandler    $ \ls ->
                      templateHandler td defaultReloadHandler $ \ts ->
              ifTop     (render     ts "index")
          <|> dir "css" (renderLess ls)
          <|> fileServe "static"

Now any requests matching `/css/foobar.css` will get served the result of
putting the file `/stylesheets/foobar.less` in your project directory through
(http://lesscss.org/)[lessc]. The output of `lessc` is cached, so it isn't
run on every request. If you want to refresh the cached stylesheets, send a
request to `/admin/reload/less`. If you want a different reload handler, pass
your own one to `lessHandler` instead of `lessReloadHandler` (or simply
`(const pass)` if you wish to have no reload handler).
