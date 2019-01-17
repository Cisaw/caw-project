{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Config where

import           Control.Applicative
import qualified Control.Exception as X
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Yaml.YamlLight

-- ./.caw.yaml: A project-specific YAML that descibes the project options
--    Fields:
--      build:   <script: filepath \n bitcode: filepath | - file1 \n - file2 ... >
--      tests:
--              - Sym1
--              - Sym2
--              - symbol: Sym3
--                simulation options: - opt1
--                                    - opt2
--      diff specs:
--              - symbol: Sym3
--                symOpts: - opt1
--                         - opt2
--                oldCommit: CommitID
--                newCommit: CommitID
--              ...

data Config =
    Config { buildMethod :: BuildMethod
           , testSymbols :: [String]
           }
  deriving (Show)

noConfig :: Config
noConfig = Config (CompileFiles []) []

data BuildMethod
        = ScriptBuild { cfgScriptFile, cfgBitcodeFile :: FilePath }
        -- ^ Compile and link using a script
        | CompileFiles [FilePath]
        -- | DeltaBuild
        --     { oldCommit :: CommitID
        --     , newCommit :: CommitID
        --     , oldFiles    :: [FilePath]
        --     , testSymbols :: [String]
        --     }
        -- ^ Compile and link an explicit list of files
        -- ... | AutoBuild  ... someday
  deriving (Show)

readConfigFile :: FilePath -> IO (Maybe Config)
readConfigFile fp =
    X.catch doread
            (\(_::X.SomeException) -> pure Nothing)
 where
 doread :: IO (Maybe Config)
 doread = convertYamlToConfig <$> parseYamlFile fp

 convertYamlToConfig :: YamlLight -> Maybe Config
 convertYamlToConfig YNil = Nothing
 convertYamlToConfig (YSeq _seq) = Nothing
 convertYamlToConfig (YStr _str) = Nothing
 convertYamlToConfig (YMap   mp) =
    do bm <- parseBuildMethod mp -- =<< Map.lookup (YStr "build") mp
       ts <- parseTestSymbols =<< (Map.lookup (YStr "tests") mp <|> pure (YSeq []))
       pure $ Config bm ts

parseBuildMethod :: Map YamlLight YamlLight -> Maybe BuildMethod
parseBuildMethod mp
    | Just (YSeq bs) <- Map.lookup (YStr "build") mp =
           CompileFiles <$> mapM getStr bs
    | Just (YMap bm) <- Map.lookup (YStr "build") mp =
        do script <- getStr =<< Map.lookup (YStr "script") bm
           out    <- getStr =<< Map.lookup (YStr "bitcode") bm
           pure $ ScriptBuild script out
    | otherwise =
        do script <- getStr =<< Map.lookup (YStr "buildscript") mp
           out    <- getStr =<< Map.lookup (YStr "bitcode") mp
           pure $ ScriptBuild script out

getStr :: YamlLight -> Maybe String
getStr (YStr s) = Just (BC.unpack s)
getStr _        = Nothing

parseTestSymbols :: YamlLight -> Maybe [String]
parseTestSymbols y
    | YSeq syms <- y = mapM getStr syms
    | otherwise = Just []
