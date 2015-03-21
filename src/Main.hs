module Main where

import Text.Pandoc
import Happstack.Server
import qualified Text.Blaze.Html4.Strict as H
import Control.Monad.IO.Class (liftIO)
import System.IO.Error (catchIOError, ioeGetErrorString)
import System.Environment (getArgs)
import Data.List (isSuffixOf)

main :: IO ()
main = do
    path <- getBasePath
    simpleHTTP nullConf $ uriRest $ serveMarkdown path

-- | Retrieve the specified path from args
getBasePath :: IO String
getBasePath = do
    args <- getArgs
    case args of
        -- Make sure the path ends with a '/'
        path:_ -> if "/" `isSuffixOf` path
                  then return path
                  else return $ path ++ "/"
        _      -> do
            putStrLn "No path specified, serving from '.'"
            return ""

-- | The main ServerPart
serveMarkdown :: FilePath -> String -> ServerPartT IO H.Html
serveMarkdown basepath name = do
    let stripped = case name of
            "/" -> "index"   -- Use index.markdown as default page
            _   -> tail name -- Removes the leading '/'
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
