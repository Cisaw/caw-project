{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ImplicitParams       #-}
-- | An LLVM build system for Caw
module Caw.BuildSystem
    ( autoBuildFiles
    ) where

import           Control.Monad (void)
import           Data.ByteString (ByteString)
import           Data.Foldable (for_,foldl')
import           Data.List (isPrefixOf)
import           Data.LLVM.BitCode
import           Data.Monoid ((<>))
import           Data.String
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Caw.BuildSystem.LanguageC as LangC
import           Lens.Micro.Platform
import           System.Directory (setCurrentDirectory)
import           System.FilePath ((</>),(<.>), takeExtension)
import           System.Process
import           Text.LLVM.AST
import           Text.LLVM.PP
import           Text.LLVM.Lens
import           Text.Show.Pretty (ppShow)

isLLVMIntrinsic :: Symbol -> Bool
isLLVMIntrinsic (Symbol s) = isPrefixOf llvmIntrinsicPrefix s
 where
  llvmIntrinsicPrefix :: String
  llvmIntrinsicPrefix = "llvm."

-- | @llvmBC dir includeDirs files@ compiles each file in @files@ (under directory @dir@)
-- into bitcode then links them all, producing a final LLVM bitcode file at
-- the returned 'FilePath'.
llvmBC :: FilePath -> [FilePath] -> [FilePath] -> FilePath -> IO ()
llvmBC _ [] _ _ = error "Trying to compile no files."
llvmBC path files includeDirs result =
    do setCurrentDirectory path
       for_ files $ \file ->
          produce "[CC]" file (bcFileOf file) (llvmEmitBC includeDirs)
       produce "[AR]" (map bcFileOf files) result llvmLink

bcFileOf :: FilePath -> FilePath
bcFileOf = (<.> "bc")

-- Some clean stdout messages for build information.
produce :: (Show a, Show b) => String -> a -> b -> (a -> b -> IO ()) -> IO ()
produce tag a b io =
  do putStr (tag ++ " " ++ show a)
     io a b
     putStrLn (" ~> " ++ show b)

llvmLink :: [FilePath] -> FilePath -> IO ()
llvmLink files out = callProcess "llvm-link" (files ++ ["-o",out])

-- |@llvmEmitBC cFile bcFile@ compiles the file at @cFile@ and produces llvm
-- bitcode saved at @bcFile@.
llvmEmitBC :: [FilePath] -> FilePath -> FilePath -> IO ()
llvmEmitBC incls file outfile
  | inext == ".ll" = callProcess "llvm-as" ["-o", outfile, file]
  | inext == ".c" =
        callProcess "clang" $ ["-c", "-emit-llvm", "-o", outfile, file] ++ concat [["-I",x] | x <- incls]
  | inext `elem` [".cc", ".cxx", ".cpp"] =
        callProcess "clang++" $ ["-c", "-emit-llvm", "-o", outfile, file] ++ concat [["-I",x] | x <- incls]
  | inext == ".rs" = callProcess "rustc" ["--emit","llvm-bc","--crate-type","lib","-o",outfile,file]
  | otherwise = error $ "The caw build system does not support files with\
                        \an extension of " <> inext
 where
     inext = takeExtension file


--------------------------------------------------------------------------------
--  High Level Full-Repo Builder

-- | Automatic as in automatic or simple to the user, which means there is
--   signficant logic here.
autoBuildFiles :: [FilePath]     -- ^ The files to build (relative paths)
               -> T.Text         -- ^ The suffix for renaming the symbols
               -> FilePath       -- ^ Absolute path to the code
               -> IO FilePath  -- ^ Resulting bitcode file
autoBuildFiles alteredFiles nameSuffix codepath =
 do let bitcodeFile = codepath </> "llvm_" <> T.unpack nameSuffix <.> "bc"
        files = Set.toList $ Set.fromList alteredFiles
    contents <- mapM T.readFile files
    incDirs  <- Set.toList <$> LangC.guessIncludeDirs contents codepath
    putStrLn $ "Guessing include directories of: " <> (show incDirs)
    llvmBC codepath files incDirs bitcodeFile
    putStrLn $ "Built: " <> show bitcodeFile
    pure bitcodeFile
