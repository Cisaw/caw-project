{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
module Generate where

import Data.Monoid ((<>))
import Data.Text as T
import Data.Text.IO as T
import System.Directory
import System.FilePath ((</>), (<.>))
import System.Environment
import qualified Control.Exception as X
import System.IO
import System.Exit

data GenerateOpts = GenerateProjectConfig FilePath | GenerateUserProfile FilePath deriving (Show)

generate :: GenerateOpts -> IO ()
generate (GenerateProjectConfig fp) = generateProjectConfig fp
generate (GenerateUserProfile fp)   = generateUserProfile fp

generateProjectConfig :: FilePath -> IO ()
generateProjectConfig fp =
 do let cont = T.unlines [ "build:"
                         , "    script: <script.sh>"
                         , "    bitcode: <filepath.bc>"
                         , "#  or let caw try to guess how to build a simple project:"
                         , "#  - file1.c"
                         , "#  - src/file2.c"
                         , "#  - other/src/file3.c"
                         , "tests:"
                         , "    - user_symbol1"
                         , "#   - user_symbol2"
                         , "#   - user_symbol3"
                         ]
    Prelude.putStrLn . ("Saved: " <>) =<< saveAsAbout fp cont

generateUserProfile :: FilePath -> IO ()
generateUserProfile path =
 do env <- getEnvironment
    fp  <- if (Prelude.take 6 path == "$HOME/")
                then case lookup "HOME" env of
                            Just h -> pure (h </> Prelude.drop 6 path)
                            Nothing -> do T.hPutStrLn stderr "Could not find HOME directory from environment."
                                          exitFailure
                else pure path
    let user = maybe "<user>" T.pack (lookup "USER" env)
        cont = T.unlines [ "user: guest       # perhaps github:" <> user
                         , "token:  <token>   # get a user name and token from https://cisaw.io"
                         , "server: https://cisaw.io"
                         , "port: 443"]
    Prelude.putStrLn . ("Saved: " <>) =<< saveAsAbout fp cont

saveAsAbout :: FilePath -> Text -> IO FilePath
saveAsAbout fp cont =
  do let fileNames = [fp <.> x | x <- "" : (show <$> [(1::Int)..100])]
     go fileNames
 where
 go [] = do T.hPutStrLn stderr $ "Could not write to file: '" <> T.pack (show fp) <> "'"
            exitFailure
 go (f:fs) =
  do let tryAgain = go fs
     b <- doesFileExist f
     if b then tryAgain
          else do ugh <- X.catch (T.writeFile f cont >> return False)
                                 (\(_::X.IOException) -> return True)
                  if ugh then tryAgain
                         else return f
