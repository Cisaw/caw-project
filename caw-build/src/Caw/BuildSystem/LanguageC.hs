{-# LANGUAGE OverloadedStrings #-}
module Caw.BuildSystem.LanguageC
    ( parse
    , TypeHint(..)
    , getFunctionTypeHint, lookupFunctionDef
    , guessIncludeDirs
    ) where

import qualified Control.Exception as X
import           Data.Char (isSpace)
import qualified Data.Foldable as F
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import           Data.Monoid ((<>))
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           Language.C
import           Language.C.System.GCC
import           System.FilePath
import           System.Directory ( doesDirectoryExist, getDirectoryContents
                                  , canonicalizePath, getCurrentDirectory
                                  , setCurrentDirectory)
import           Text.LLVM

parse :: [FilePath] -> FilePath -> IO (Either ParseError CTranslUnit)
parse incls file = parseCFile (newGCC "gcc") Nothing (("-I"<>) <$> incls) file

-- LLVM Types drop information that is available in the C soruce, such as array
-- lengths.  We can cheaply grab just enough information from the source to aid
-- SAW generation of simple structures, such as arrays.
data TypeHint = ArrayOfSize Int | NoHint
        deriving (Eq,Ord,Show)

-- Obtain the LLVM type for a function as well as the argument names.
getFunctionTypeHint :: Show a => CFunctionDef a -> [TypeHint]
getFunctionTypeHint (CFunDef _retTys declr _ _ _) =
    case declr of
      CDeclr _ ds _ _ _ ->
        case ds of
            [CFunDeclr (Right (cds,_)) _ _] -> map typeHintOfCDecl cds
            _ -> error "Could not obtain function arg types from parse."

-- This function processes language-c AST and produces the matching LLVM type.
-- It slightly (ab)uses the `Type` type by returning Array's for C arrays when
-- in fact those are compiled to LLVM Ptr types, but the value captures
-- the necessary information (array length) and is a correct abstraction.
typeHintOfCDecl :: Show a => CDeclaration a -> TypeHint
typeHintOfCDecl (CDecl ty1s ty2 _) =
    let _subTy = typeOfTypeSpecs ty1s
    in case ty2 of
        [] -> NoHint
        [(Just (CDeclr _ (CArrDeclr _ cArrSz _ : _ ) _ _ _),_,_)] ->
            let sz = arraySizeToInt cArrSz
            in ArrayOfSize sz
        _ -> NoHint
typeHintOfCDecl _ = NoHint -- "Unrecognized CDecl"

arraySizeToInt :: (Show a, Integral int) => CArraySize a -> int
arraySizeToInt (CNoArrSize _) = error "Array with no size information in the source."
arraySizeToInt (CArrSize _ expr) =
    case expr of
        (CConst (CIntConst (CInteger num _ _) _)) -> fromIntegral num
        _ -> error "Array size is not a literal." -- XXX evaluate.

typeOfTypeSpecs :: Show a => [CDeclarationSpecifier a] -> Type
typeOfTypeSpecs [CTypeSpec (CUnsigType _), CTypeSpec ty] = typeOfTypeSpec ty
typeOfTypeSpecs [CTypeSpec (CSignedType _), CTypeSpec ty] = typeOfTypeSpec ty
typeOfTypeSpecs [CTypeSpec ty] = typeOfTypeSpec ty
typeOfTypeSpecs xs  = error $ "Unrecognized type: " ++ show xs

typeOfTypeSpec :: CTypeSpecifier a -> Type
typeOfTypeSpec ty =
    PrimType $ case ty of
        CVoidType    _        -> Void
        CCharType    _        -> Integer 8
        CShortType   _        -> Integer 16
        CLongType    _        -> Integer 32
        CFloatType   _        -> FloatType Float
        CDoubleType  _        -> FloatType Double
        CSignedType  _        -> Integer 32
        CUnsigType   _        -> Integer 32
        CBoolType    _        -> Integer 1
        CIntType     _        -> Integer 32
        CInt128Type  _        -> Integer 128
        -- XXX Support more types
        CComplexType _        -> error "Complex types not supported"
        CSUType      _st _    -> error "structure types not supported."
        CEnumType    _   _    -> error "C type not supported"
        CTypeDef     _   _    -> error "C type not supported"
        CTypeOfExpr  _   _    -> error "C type not supported"
        CTypeOfType  _   _    -> error "C type not supported"
        CAtomicType  _   _    -> error "C type not supported"
        CFloat128Type _       -> error "C type not supported"



lookupFunctionDef :: Symbol -> CTranslUnit -> Maybe (CFunctionDef NodeInfo)
lookupFunctionDef sym (CTranslUnit es _) =
    case filter (isCFDefOf sym) es of
      CFDefExt d : _ -> Just d
      _              -> Nothing

isCFDefOf :: Symbol -> CExternalDeclaration a -> Bool
isCFDefOf func (CFDefExt (CFunDef _ decl _ _ _)) =
    isCDeclOf func decl
isCFDefOf _ _ = False

isCDeclOf :: Symbol -> CDeclarator a -> Bool
isCDeclOf sym (CDeclr (Just ident) _ _ _ _) =
    let str = identToString ident
    in fromString str == sym
isCDeclOf _ _ = False

guessIncludeDirs :: [Text] -> FilePath -> IO (Set.Set FilePath)
guessIncludeDirs fileContents top =
  do cwd <- getCurrentDirectory
     setCurrentDirectory top
     let includes         = mconcat $ map getIncludes fileContents
         includeFileDepth = Map.fromList (map (\f -> (takeFileName f, reverse (splitPath f))) (Set.toList includes))
         mkCandidate x =
            case Map.lookup (takeFileName x) includeFileDepth of
                Nothing -> Nothing
                Just [_]  -> Just $ takeDirectory x
                Just ps | ps == take psLen (reverse splits) ->
                        Just (joinPath (take (length splits - psLen) splits))
                    where splits = splitPath x
                          psLen  = length ps
                Just _ -> Nothing
     getCandidatePaths mkCandidate =<< canonicalizePath top
        `X.finally` (setCurrentDirectory cwd)
 where

 getIncludes :: Text -> (Set.Set FilePath)
 getIncludes cont =
    let ls = T.lines cont
        inc = filter ("#include" `T.isPrefixOf`) (map (T.dropWhile isSpace) ls)
        incWithQuote = filter (T.any (== '"')) inc
        cleanQuotes = T.unpack . T.takeWhile (/='"') . T.drop 1 . T.dropWhile (/= '"')
        incWithAngBrackets = filter (T.any (== '<')) inc
        cleanAB = T.unpack . T.takeWhile (/='>') . T.drop 1 . T.dropWhile (/= '<')
    in Set.fromList (map cleanQuotes incWithQuote ++ map cleanAB incWithAngBrackets)

 getCandidatePaths :: (FilePath -> Maybe FilePath) -> FilePath -> IO (Set.Set FilePath)
 getCandidatePaths mkCandidate currDir =
    do let noSpecial = filter (not . (`elem` [".",".."]))
       xs <- noSpecial <$> getDirectoryContents currDir
       (psAll,fs) <- partitionM doesDirectoryExist ((currDir </>) <$> xs)
       ps <- mapM canonicalizePath psAll
       let rest = mapM (getCandidatePaths mkCandidate) ps
       candidates <- mapM canonicalizePath (catMaybes $ map mkCandidate fs)
       Set.unions . (Set.fromList candidates :) <$> rest

partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM f list = go list mempty mempty
 where
    go [] ys zs = return (F.toList ys,F.toList zs)
    go (x:xs) ys zs =
      do b <- f x
         if b
            then go xs (ys Seq.|> x)  zs
            else go xs  ys           (zs Seq.|> x)
