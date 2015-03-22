{-# LANGUAGE OverloadedStrings #-}

module Configuration where

import Data.Aeson
import Control.Applicative ((<$>),(<*>))
import Control.Monad (mzero)
import System.Directory (doesFileExist)
import qualified Data.ByteString.Lazy as B (readFile)
import System.Exit (exitFailure)

-- | Contains the configuratio values
data Configuration = Configuration {
    -- | The CSS style sheet
      style :: Maybe FilePath
    -- | The port to serve the wiki on
    , servePort  :: Int
    -- | The index page
    , index :: String
    }

instance FromJSON Configuration where
    parseJSON (Object v) = Configuration <$>
        v .:? "style" .!= style defaultConfiguration <*>
        v .:? "port"  .!= servePort defaultConfiguration <*>
        v .:? "index" .!= index defaultConfiguration

    parseJSON _ = mzero

-- | The default configuration
defaultConfiguration :: Configuration
defaultConfiguration = Configuration {
      style     = Nothing
    , servePort = 8000
    , index     = "index"
    }

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
                putStrLn "Couldn't parse configuration file"
                exitFailure
