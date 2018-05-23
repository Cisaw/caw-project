{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
module Build
    ( simpleBuildFiles, buildBitcodeViaConfig
    , BuildOpts(..), BuildStyle(..)
    , build
    ) where

import qualified Caw.BuildSystem as Build
import qualified Control.Exception as X
import qualified Data.ByteString as BS
import           Data.Semigroup((<>))
import           System.Directory (doesFileExist,getCurrentDirectory)
import           System.FilePath ((</>))
import           System.IO
import           System.IO.Temp
import           System.Exit
import           System.Process

import Config

data BuildOpts =
      BuildOpts
        { boConfigFile  :: FilePath
        , boProfileFile :: Maybe FilePath
        , boBuildStyle  :: BuildStyle
        }
        deriving (Show)

data BuildStyle = AutoBuild | BuildFiles [FilePath] deriving (Show)

build :: BuildOpts -> IO ()
build opts =
  do mcfg <- readConfigFile (boConfigFile opts)
     case boBuildStyle opts of
        AutoBuild ->
            case mcfg of
                Nothing  ->
                 do hPutStrLn stderr "Could not read project config and no files specified for build."
                    exitFailure
                Just cfg ->
                 do fp <- buildBitcodeViaConfig cfg
                    announce fp
        BuildFiles fs ->
         do tmp <- getCanonicalTemporaryDirectory
            (path,hdl) <- openTempFile tmp "caw-autobuild.bc"
            BS.hPut hdl =<< BS.readFile =<< simpleBuildFiles fs
            hClose hdl
            announce path

announce :: FilePath -> IO ()
announce = putStrLn . ("wrote: " <>)

simpleBuildFiles :: [FilePath] -> IO FilePath
simpleBuildFiles fs =
     Build.autoBuildFiles fs "autobuild" =<< getCurrentDirectory

buildBitcodeViaConfig :: Config -> IO FilePath
buildBitcodeViaConfig cfg =
    case buildMethod cfg of
        ScriptBuild scriptfile bitfile ->
            doesFileExist bitfile >>= \case
                True  -> pure bitfile
                False -> do executeScriptOrExit scriptfile
                            pure bitfile
        CompileFiles cf -> simpleBuildFiles cf

-- Executes the script and pretty prints the error (if any). Also terminates if
-- the script returns non-zero.
executeScriptOrExit :: FilePath -> IO ()
executeScriptOrExit scriptfile =
  do currDir <- getCurrentDirectory
     let create_proc = (proc (currDir </> scriptfile) []) { create_group = True }
     (e,so,se) <- readCreateProcessWithExitCode create_proc ""
                    `X.catch` hdlr
     case e of
             ExitSuccess   -> return ()
             ExitFailure i ->
              do hPutStrLn stderr $ "Build script failed (non-zero exit code '" <> show i <> "')"
                 hPutStrLn stderr "Stderr and stdout: "
                 hPutStrLn stderr "--------- STDERR ---------"
                 hPutStrLn stderr se
                 hPutStrLn stderr "--------- STDOUT ---------"
                 hPutStrLn stderr so
                 exitFailure
 where
   hdlr :: X.SomeException -> IO a
   hdlr e =
    do putStrLn $ "Error: " <> show e
       putStrLn =<< getCurrentDirectory
       exitFailure
