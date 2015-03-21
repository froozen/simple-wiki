{-# LANGUAGE OverloadedStrings #-}

module Serving where

import Happstack.Server
import Control.Monad.Reader
import Control.Monad.IO.Class (liftIO)
import Text.Pandoc
import Text.Blaze ((!))
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A
import System.Directory (doesFileExist)
import System.IO.Error (catchIOError, ioeGetErrorString)

import Configuration

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
    converted <- liftIO $ convertFile $ name ++ ".markdown"
    config <- ask
    ok $ toResponse $ webPage name (style config) converted

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
convertFile :: FilePath -> IO H.Html
convertFile fp = do
    content <- readFile fp
    return $ writeHtml def $ readMarkdown def content
