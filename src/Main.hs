module Main where

import Happstack.Server
import Control.Monad.Reader
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import Data.List (isSuffixOf)

import Configuration
import Serving

main :: IO ()
main = do
    path <- getBasePath
    config <- loadConfiguration path
    simpleHTTP (nullConf { port = servePort config }) $
        uriRest $ \uri -> runReaderT (serve path uri) config

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
