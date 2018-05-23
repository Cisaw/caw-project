{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
module Main where

import Options.Applicative
import Data.Semigroup((<>))
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Text.Read

import Verify
import Generate
import Build

-- Help:
-- caw verify <symbols> [-c <file>] [-h <host>] [-p <port>] [-b <file>]
-- caw generate config [FILE]
-- caw generate profile [FILE]
-- caw build ['auto' | FILE ]
topParser :: Parser Command
topParser =
  hsubparser
    (  command "verify"  (info (Verify <$> verifyParser <**> helper) (progDesc verifyDesc))
    <> command "generate" (info (generateParser <**> helper) (progDesc genDesc))
    <> command "build"    (info (buildParser <**> helper) (progDesc buildDesc))
    )
 where
 verifyDesc = "Verify a function returns true for all inputs"
 genDesc    = "Generate an empty configuration"
 buildDesc  = "Build the bitcode necessary for verification"

buildParser :: Parser Command
buildParser = Build <$>
    hsubparser ( command "auto" (info autoBuildParser (progDesc autoDesc))
              <> command "these" (info fileBuildParser (progDesc fileDesc))
               )
 where
    autoDesc = "Use the project-specified build system"
    fileDesc = "Compile and link the specified files, emitting \
                   \the bitcode path if successful."
    autoBuildParser :: Parser BuildOpts
    autoBuildParser  = BuildOpts <$> parseConfig <*> parseProfile <*> pure AutoBuild
    fileBuildParser :: Parser BuildOpts
    fileBuildParser =
        BuildOpts <$> parseConfig <*> parseProfile
              <*> (BuildFiles <$> some (argument str (metavar "FILES")))

generateParser :: Parser Command
generateParser = Generate <$>
    hsubparser ( command "config" (info ((GenerateProjectConfig <$> cfgFile)
                                      <**> helper)
                                      (progDesc "Generate a project configuration (.caw.yaml) file"))
             <> command "profile" (info ((GenerateUserProfile <$> proFile)
                                      <**> helper)
                                      (progDesc "Generate the user ($HOME/.caw.yaml) file"))
             )
 where cfgFile = argument str (metavar "FILE" <> value ".caw.yaml")
       proFile = argument str (metavar "FILE" <> value "$HOME/.caw.yaml")

parseConfig :: Parser FilePath
parseConfig =
   strOption
        (  long "config"
        <> short 'c'
        <> metavar "FILEPATH"
        <> value ".caw.yaml"
        <> help "Configuration file (default: ./.caw.yaml)" )

parseProfile :: Parser (Maybe FilePath)
parseProfile =
   option (Just <$> str)
        (  long "user-profile"
        <> short 'u'
        <> metavar "FILEPATH"
        <> value Nothing
        <> help "Profile filepath" )

parseVerbosity :: Parser Verbosity
parseVerbosity =
    option (maybeReader readIt)
      (  long "verbosity"
      <> short 'v'
      <> metavar "INT"
      <> value ERROR
      <> help "Verbosity level from 0 to 3 (slient, error, warn, debug)")
  where
    readIt :: String -> Maybe Verbosity
    readIt s =
        case readMaybe s :: Maybe Int of
            Just 0  -> Just SILENT
            Just 1  -> Just ERROR
            Just 2  -> Just WARN
            Just n | n >= 3  -> Just DEBUG
            _       -> Nothing

verifyParser :: Parser VerifyOpts
verifyParser =
    VerifyOpts
           <$> parseConfig
           <*> parseProfile
           <*> option (Just <$> str)
                (  long "host"
                <> short 'h'
                <> metavar "HOST"
                <> value Nothing
                <> help "URL of the Cisaw server (default: cisaw.io)" )
           <*> option (maybeReader readMaybe)
                (  long "port"
                <> short 'p'
                <> metavar "PORT"
                <> value Nothing
                <> help "Cisaw server port (default: 8080)" )
           <*> parseVerification
           <*> parseVerbosity
  where

parseVerification :: Parser Verification
parseVerification =
    hsubparser ( command "eq"   (info (Equivalence <$> parseEquivalence) (progDesc eqDesc))
              <> command "diff" (info (Differential <$> parseDifferential) (progDesc diffDesc))
              <> command "prop" (info (Property <$>  parseProp) (progDesc vDesc)))
              <|> pure (Property (Prop mempty mempty))
 where
 vDesc     = "Property: Verify each identified function returns 1 for all inputs. (DEFAULT)"
 eqDesc    = "Equivalence: Verify two functions produce the same result for all inputs."
 diffDesc  = "Difference: Verify a property which refers to code in disparate commits."
 parseProp = Prop <$> parseBitcode <*> parseSymbols
 parseBitcode =
           option (Just <$> str)
                (  long "bitcode"
                <> short 'b'
                <> metavar "FILEPATH"
                <> value Nothing
                <> help "Path to an existing bitcode file"
                )
 parseSymbols = many (argument str (metavar "<SYMBOL 1 .. SYMBOL N>"))

parseEquivalence :: Parser Equivalence
parseEquivalence =
    Equiv <$> strOption
                (  long "commit1"
                <> short 'p'
                <> metavar "<Previous Commit>"
                <> value "master"
                <> help "Commit of source containing the first symbol."
                )
         <*> strOption
                (  long "symbol1"
                <> short 's'
                <> metavar "<symbol>"
                <> value "test"
                <> help "Name of the symbol being compared."
                )
         <*> option filesList
                (  long "files1"
                <> short 'f'
                <> metavar "<file1,file2,file3>"
                <> value []
                <> help "Files to compile if not using the script in .caw.yaml"
                )
         <*> strOption
                (  long "commit2"
                <> short 'n'
                <> metavar "<New Commit>"
                <> value "master"
                <> help "Commit of source containing the first symbol."
                )
         <*> strOption
                (  long "symbol2"
                <> short 'y'
                <> metavar "<symbol>"
                <> value "test"
                <> help "Name of the symbol being compared."
                )
         <*> option filesList
                (  long "files2"
                <> short 'l'
                <> metavar "<file1,file2,file3>"
                <> value []
                <> help "Files to compile if not using the script in .caw.yaml"
                )
        <|> argument (eitherReader (A.parseOnly parseEquivPair . T.pack))
                (  metavar "<commit1/symbol1:commit2/symbol2>"
                <> help "Specify the commits and symbols as a single string"
                )
  where
  parseEquivPair :: A.Parser Equivalence
  parseEquivPair = do
       c1 <- A.takeWhile1 (/= '/')
       _  <- A.char '/'
       s1 <- A.takeWhile1 (/= ':')
       _  <- A.char ':'
       c2 <- A.takeWhile1 (/= '/')
       _  <- A.char '/'
       s2 <- A.takeText
       pure $ Equiv c1 s1 [] c2 s2 []

filesList :: ReadM [FilePath]
filesList =
    let go p  []  | Seq.null p  = Just []
                  | otherwise   = Just [F.toList p]
        go p  ('\\' : ',' : xs) = go (p Seq.|> ',') xs
        go p  (',' : xs)        = (F.toList p :) <$> go mempty xs
        go p  (x:xs)            = go (p Seq.|> x) xs
    in maybeReader (go mempty)

parseDifferential :: Parser Differential
parseDifferential =
    Diff <$> strOption
                (  long "commit1"
                <> short 'p'
                <> metavar "<Previous Commit>"
                <> help "Commit of source containing the 'old' code."
                )
         <*> option filesList
                (  long "files1"
                <> short 'f'
                <> metavar "<file1,file2,file3>"
                <> value []
                <> help "Files to compile if not using the script in .caw.yaml"
                )
         <*> strOption
                (  long "specCommit"
                <> short 's'
                <> metavar "<Commit containing the specification>"
                <> value "HEAD"
                <> help "Commit of the source containing the specification."
                )
         <*> strOption
                (  long "specSymbol"
                <> short 'y'
                <> metavar "<test function name>"
                <> help "Symbol of the test function"
                )
         <*> option filesList
                (  long "spec-files"
                <> short 'p'
                <> metavar "<file1,file2,file3>"
                <> value []
                <> help "Files to compile if not using the script in .caw.yaml"
                )
         <*> strOption
                (  long "commit2"
                <> short 'n'
                <> metavar "<New Commit>"
                <> help "Commit of source containing the 'new' code."
                )
         <*> option filesList
                (  long "files2"
                <> short 'l'
                <> metavar "<file1,file2,file3>"
                <> value []
                <> help "Files to compile if not using the script in .caw.yaml"
                )
        <|> argument (eitherReader (A.parseOnly parseDiffTriple . T.pack))
                (  metavar "<commit1:commitSpec/symbolSpec:commit2>"
                <> help "Specify the commits and symbol as a single string"
                )
   where
    parseDiffTriple :: A.Parser Differential
    parseDiffTriple = do
     commitNr1   <- A.takeWhile1 (/= ':')
     _           <- A.char ':'
     commitSpec  <- A.takeWhile1 (/= '/')
     _           <- A.char '/'
     symbolSpec  <- A.takeWhile1 (/= ':')
     _           <- A.char ':'
     commitNr2   <- A.takeText
     pure $ Diff commitNr1 [] commitSpec symbolSpec [] commitNr2 []

data Command
    = Verify VerifyOpts
    | Generate GenerateOpts
    | Build BuildOpts
    deriving (Show)

main :: IO ()
main =
  do let runParse = customExecParser optPrefs
         optPrefs = prefs (disambiguate <> showHelpOnError <> showHelpOnEmpty)
         opts     = info (topParser <**> helper) fullDesc
     cmd <- runParse opts
     case cmd of
      Verify   vo -> verify vo
      Generate go -> generate go
      Build    bo -> build bo

-- Help:
-- caw verify <symbols> [-c <file>] [-h <host>] [-p <port>] [-b <file>]
-- caw generate config [FILE]
-- caw generate profile [FILE]
-- caw build ['auto' | FILE ]

-- Files:
--
