{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Text.Pandoc
import Happstack.Server
import Control.Applicative ((<$>),(<*>))
import Control.Monad (mzero)
import Control.Monad.Reader
import Text.Blaze ((!))
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A
import Control.Monad.IO.Class (liftIO)
import System.IO.Error (catchIOError, ioeGetErrorString)
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import Data.List (isSuffixOf)
import qualified Data.ByteString.Lazy as B (readFile)

data Configuration = Configuration {
    -- | The CSS style sheet
      style :: Maybe FilePath
    -- | The port to serve the wiki on
    , servePort  :: Int
    -- | The index page
    , index :: String
    }

-- | The default configuration
defaultConfiguration :: Configuration
defaultConfiguration = Configuration {
      style     = Nothing
    , servePort = 8000
    , index     = "index"
    }

instance FromJSON Configuration where
    parseJSON (Object v) = Configuration <$>
        v .:? "style" .!= style defaultConfiguration <*>
        v .:? "port"  .!= servePort defaultConfiguration <*>
        v .:? "index" .!= index defaultConfiguration

    parseJSON _ = mzero

main :: IO ()
main = do
    path <- getBasePath
    config <- loadConfiguration path
    simpleHTTP (nullConf { port = servePort config }) $
        uriRest $ \uri -> runReaderT (serve path uri) config

-- | Load the configuration from a file
loadConfiguration :: FilePath -> IO Configuration
loadConfiguration path = do
    let configPath = path ++ "simple-wiki.json"
    exists <- doesFileExist configPath
    if not exists
    then return defaultConfiguration
    else do
        configFile <- B.readFile configPath
        case decode configFile of
            Just config -> return config
            _           -> do
                putStrLn $ "Couldn't parse configuration file"
                exitFailure

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
serve :: FilePath -> String -> ReaderT Configuration (ServerPartT IO) Response
serve basepath name = do
    config <- ask
    let stripped = case name of
            "/" -> index config   -- Use the configured index
            _   -> tail name      -- Removes the leading '/'
    -- Markdown files get preference when serving
    exists <- liftIO $ doesFileExist $ stripped ++ ".markdown"
    if exists
    then serveMarkdown basepath stripped
    else serveDirectory DisableBrowsing [name] basepath

-- | The ServerPart responsible for serving parsed markdowns
serveMarkdown :: FilePath -> FilePath -> ReaderT Configuration (ServerPartT IO) Response
serveMarkdown basepath name = do
    parsed <- liftIO $ readFromFile $ name ++ ".markdown"
    case parsed of
        Right html -> do
            config <- ask
            ok $ toResponse $ webPage name (style config) html
        Left err -> ok $ toResponse err

-- | The basic blaze-html tempalte for a website
webPage :: String -> Maybe String -> H.Html -> H.Html
webPage title style content =
    H.html $ do
        H.head $ do
            H.title (H.toHtml title)
            case style of
                Just css -> H.link ! A.rel "stylesheet"
                                   ! A.href (H.toValue css)
                -- An empty link, as there seems to be no empty Html value
                _ -> H.link
        H.body $ do
            content

-- | Read from a markdown file and convert its contents to html
readFromFile :: FilePath -> IO (Either String H.Html)
readFromFile fp = (do
        content <- readFile fp
        return . Right $ writeHtml def $ readMarkdown def content
    ) `catchIOError`
        (\e -> return $ Left $ "IOError: " ++ ioeGetErrorString e)
