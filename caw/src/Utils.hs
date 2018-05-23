{-# LANGUAGE ScopedTypeVariables #-}
module Utils where

import qualified Control.Exception as X
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Semigroup((<>))
import           System.IO
import           System.IO.Error
import           System.Exit

import           Profile
import           Config

readConfigAndProfile :: FilePath -> Maybe FilePath -> IO (Config,Profile)
readConfigAndProfile cFP pFP =
  do let cfgErr   = "Could not parse the project configuration file: '" <> cFP <> "'"
         mCfg     = readConfigFile cFP
         getProfile =
            do p <- maybe findAndReadProfileFile readProfileFile pFP
               case p of
                Nothing ->
                    do putStrLn "Using default (guest) profile!  Consider saving a profile to $HOME/.caw.yaml"
                       pure defaultProfile
                Just x -> pure x
     projCfg  <- loadOrFail mCfg cfgErr
     profile  <- getProfile
     return (projCfg,profile)

loadOrFail :: IO (Maybe a) -> String -> IO a
loadOrFail r e = maybe (hPutStrLn stderr e >> exitFailure) pure =<< r

readFileOrExit :: FilePath -> String -> IO ByteString
readFileOrExit file err =
    X.catch (BS.readFile file)
            (\(e::X.IOException) ->
                do hPutStrLn stderr err
                   hPutStrLn stderr (prettyError e)
                   exitFailure)
  where prettyError :: X.IOException -> String
        prettyError e
            | isDoesNotExistError e = "File does not exist."
            | isAlreadyInUseError e = "File in use."
            | isPermissionError e   = "Insufficient permissions."
            | otherwise = "Unexpected error: " <> show e


