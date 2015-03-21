module Main where

import Text.Pandoc
import Happstack.Server
import qualified Text.Blaze.Html4.Strict as H
import Control.Monad.IO.Class (liftIO)
import System.IO.Error (catchIOError, ioeGetErrorString)

main :: IO ()
main = simpleHTTP nullConf $ uriRest serveMarkdown

-- | The main ServerPart
serveMarkdown :: String -> ServerPartT IO H.Html
serveMarkdown name = do
    let stripped = tail name -- Removes the leading '/'
    parsed <- liftIO $ readFromFile $ stripped ++ ".markdown"
    case parsed of
        Right html -> ok $ webPage name html
        Left err -> ok $ H.toHtml err

-- | The basic blaze-html tempalte for a website
webPage :: String -> H.Html -> H.Html
webPage title content =
    H.html $ do
        H.head $ do
            H.title (H.toHtml title)
        H.body $ do
            content

-- | Read from a markdown file and convert its contents to html
readFromFile :: FilePath -> IO (Either String H.Html)
readFromFile fp = (do
        content <- readFile fp
        return . Right $ writeHtml def $ readMarkdown def content
    ) `catchIOError`
        (\e -> return $ Left $ "IOError: " ++ ioeGetErrorString e)
