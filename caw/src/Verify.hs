{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}
module Verify
    ( verify
    , VerifyOpts(..), Commit, Symbol
    , Verification(..), Equivalence(..)
    , Property(..)
    , Differential(..)
    , Verbosity(..)
    ) where

import           Caw.Types
import           Control.Concurrent (threadDelay)
import           Control.Exception as X
import           Control.Monad (when)
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import           Data.Maybe (fromMaybe)
import           Data.Semigroup((<>))
import           Data.Foldable (traverse_)
import           Network.Socket
import           System.Exit
import           System.IO
import           Servant.Client.Core.Internal.Request( ServantError(..)
                                                     , responseStatusCode)
import           Network.HTTP.Types.Status(Status(..))
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Text.Prettyprint.Doc
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as Map

import           Config
import qualified Profile as P
import           CisawClient -- REST interface calls
import           Utils
import           Build as CawBuild
import           Repo as Repo

data VerifyOpts =
    VerifyOpts
        { configFile   :: FilePath
        , profileFile  :: Maybe FilePath
        , cisawHost    :: Maybe HostName
        , cisawPort    :: Maybe Int
        , verification :: Verification
        , verbosity    :: Verbosity
        } deriving (Show)

data Verification = Property Property | Equivalence Equivalence | Differential Differential
    deriving (Show)

data Property = Prop { bitcodeFile :: Maybe FilePath, symbols :: [String] }
    deriving (Show)

-- An Equivalence specification.
--      * At each commit there must be a valid .caw with build information if
--        desired.  Auto will be used if there are specified files.
--      * The symbols must exist in the build result
data Equivalence = Equiv { eqCommit1 :: Commit
                         , eqSymbol1 :: Symbol
                         , eqFiles1  :: [FilePath]
                         , eqCommit2 :: Commit
                         , eqSymbol2 :: Symbol
                         , eqFiles2  :: [FilePath]
                         }
        deriving (Show)

data Differential = Diff { diffCommit1    :: Commit
                         , diffFiles1     :: [FilePath]
                         , diffCommitSpec :: Commit
                         , diffSymbol     :: Symbol
                         , diffFilesSpec  :: [FilePath]
                         , diffCommit2    :: Commit
                         , diffFiles2     :: [FilePath]
                         }
        deriving (Show)

type Commit = Text
type Symbol = Text

data Verbosity = DEBUG | WARN | ERROR | SILENT
    deriving (Eq,Ord,Show)

dbgOut, warnOut, errOut :: VerifyOpts -> String -> IO ()
dbgOut vo s = printOut DEBUG (verbosity vo) s
warnOut vo s = printOut WARN (verbosity vo) s
errOut vo s = printOut ERROR (verbosity vo) s

printOut :: Verbosity -> Verbosity -> String -> IO ()
printOut lvl curr  str
    | lvl >= curr = hPutStrLn stderr str
    | otherwise   = pure ()

verify :: VerifyOpts -> IO ()
verify opts =
     case verification opts of
        Property p     -> verifyProperty opts p
        Equivalence e  -> verifyEquivalence opts e
        Differential d -> verifyDifferential opts d

verifyDifferential :: VerifyOpts -> Differential -> IO ()
verifyDifferential opts diff =
  do (cfg,prof) <- readConfigAndProfile (configFile opts) (profileFile opts)
     Repo.checkout opts (diffCommit1 diff)
     commit1Config <- maybe cfg id <$> readConfigFile (configFile opts)
     bitcode1 <- getBitcodeWithFiles opts Nothing (diffFiles1 diff) commit1Config
     Repo.checkout opts (diffCommit2 diff)
     commit2Config <- maybe cfg id <$> readConfigFile (configFile opts)
     bitcode2 <- getBitcodeWithFiles opts Nothing (diffFiles2 diff) commit2Config
     Repo.checkout opts (diffCommitSpec diff)
     specConfig <- maybe cfg id <$> readConfigFile (configFile opts)
     bitcodeSpec <- getBitcodeWithFiles opts Nothing (diffFilesSpec diff) specConfig
     let (host,port) = pickServer opts prof
         auth = Auth (P.username prof) (P.secretToken prof)
         task = NewJob $ NewDiffJobInfo (diffSymbol diff) auth BitcodePush
         bits = DiffPropBitcode bitcode1 bitcodeSpec bitcode2
     eresp <- CisawClient.runTickets host port [task] bits
     resp  <- case eresp of
                Left err -> do errOut opts $ "Ticket submission failed for diff job: " <> show err
                               exitFailure
                Right [r] -> pure r
                Right _rs -> do errOut opts $ "Received more than one repsonse to a single job."
                                exitFailure
     dbgOut opts $ "Ticket: " ++ show (assignedTicket resp)
     let env = Map.fromList [(assignedTicket resp, T.unpack (diffSymbol diff))]
     manageTickets opts host port [resp] env

verifyEquivalence :: VerifyOpts -> Equivalence -> IO ()
verifyEquivalence opts equiv =
  do (cfg,prof) <- readConfigAndProfile (configFile opts) (profileFile opts)
     Repo.checkout opts (eqCommit1 equiv)
     commit1Config <- maybe cfg id <$> readConfigFile (configFile opts)
     bitcode1 <- getBitcode opts Nothing commit1Config
     Repo.checkout opts (eqCommit2 equiv)
     commit2Config <- maybe cfg id <$> readConfigFile (configFile opts)
     bitcode2 <- getBitcode opts Nothing commit2Config
     let (host,port) = pickServer opts prof
         auth = Auth (P.username prof) (P.secretToken prof)
         s1   = eqSymbol1 equiv
         s2   = eqSymbol2 equiv
         task = NewJob $ NewEquivJobInfo s1 s2 auth BitcodePush
     eresps <- CisawClient.runTickets host port [task] (EquivBitcode bitcode1 bitcode2)
     resp   <- case eresps of
                Left err -> do errOut opts $ "Ticket submission failed for equiv job: " <> show err
                               exitFailure
                Right [r] -> pure r
                Right _rs -> do errOut opts $ "Received more than one response to a single job."
                                exitFailure
     let env = Map.fromList [(assignedTicket resp, T.unpack $ T.unwords [s1, "~", s2])]
     dbgOut opts $ "Ticket: " ++ show (assignedTicket resp)
     manageTickets opts host port [resp] env


verifyProperty :: VerifyOpts -> Property -> IO ()
verifyProperty opts prop =
  do (cfg,prof) <- readConfigAndProfile (configFile opts) (profileFile opts)
     bitcodeBytes   <- getBitcode opts (bitcodeFile prop) cfg
     let theSymbols | null (symbols prop) = testSymbols cfg
                    | otherwise           = symbols prop
     let (host,port) = pickServer opts prof
         auth  = Auth (P.username prof) (P.secretToken prof)
         tasks = [NewJob $ NewPropJobInfo (T.pack testSym)
                                          [] [] auth BitcodePush
                                    | testSym <- theSymbols]
     when (null tasks) $
        do errOut opts "No test symbols found either from the command line or \
                       \the configuration file."
           exitFailure
     eresps <- CisawClient.runTickets host port tasks $ PropBitcode bitcodeBytes
     resps <- case eresps of
                Left (FailureResponse r)
                    | statusCode (responseStatusCode r) == 400 ->
                            do errOut opts $ "Server responded with HTTP code \
                                             \400.  Did you use HTTP instead \
                                             \of HTTPS in your $HOME/.caw.yaml?"
                               exitFailure
                Left (ConnectionError str) ->
                    do errOut opts "Connection error.  Check that the profile \
                                   \has https for the server and port 443.\
                                   \  Alternatively, the server could be down."
                       dbgOut opts ("Message: " ++ show str)
                       exitFailure
                Left err ->
                    do errOut opts $ "Ticket submission failed: " <> show err
                       exitFailure
                Right r -> pure r
     let env = Map.fromList (zip (map assignedTicket resps) theSymbols)
     dbgOut opts $ "Tickets: " ++ show (Map.toList env)
     manageTickets opts host port resps env

manageTickets :: VerifyOpts -> HostName -> Int -> [Response] -> Map.Map Ticket String -> IO ()
manageTickets opts host port resps env = queries resps 100000
 where
 minDelay = 1000*1000
 maxDelay = 60*1000*1000
 queries :: [Response] -> Int -> IO ()
 queries [] _ = pure ()
 queries old delay =
    do dbgOut opts $ unlines (map (\o -> show (assignedTicket o) ++ " : " ++ show (jobStatus o)) old)
       threadDelay delay
       ers <- runReq host port $ traverse (submitTask . PollJob . assignedTicket) old
       case ers of
        Left err ->
            do warnOut opts $ "Could not understand server response when polling ticket:\n\t" ++ show err
               queries old (calcDelay delay)
        Right rs -> handleQueryResult old rs delay
 handleQueryResult :: [Response] -> [Response] -> Int -> IO ()
 handleQueryResult old rs delay =
    do traverse_ reportChangedStatus (zip old rs)
       let remainingTickets = filter notDone rs
           newDelay | or (zipWith (/=) old rs) = minDelay
                    | otherwise                = calcDelay delay
       queries remainingTickets newDelay
 calcDelay = min maxDelay . (floor :: Double -> Int) . (* 1.1) . fromIntegral
 notDone :: Response -> Bool
 notDone r = case jobStatus r of
               Other _t    -> False
               ReceivedJob -> True
               ReceivedBitcode o ->
                case o of
                    Failed  {}   -> False
                    QuickChecked -> False
                    Proven       -> False
                    Unknown      -> False
                    InProgress _ -> True

 reportChangedStatus :: (Response, Response) -> IO ()
 reportChangedStatus (rOld,rNew) =
    let msym = Map.lookup (assignedTicket rNew) env
    in when (rOld /= rNew) (reportStatus (jobStatus rNew) msym)

 reportStatus r Nothing =
        do errOut opts $ "SERVER ERROR: Returned ticket does not match any known job."
           errOut opts $ "Env: " <> show env
           errOut opts $ "Response: " <> show r
 reportStatus r (Just sym) =
    case r of
        ReceivedJob -> return ()
        Other t     -> alertUser red sym t
        ReceivedBitcode o ->
            case o of
                Failed  ce   ->
                    do alertUser red    sym "Failed"
                       putStrLn $ unlines $ map (T.unpack . ("\t"<>)) ce
                QuickChecked -> alertUser pink   sym "QuickChecked"
                Proven       -> alertUser green  sym "Proven"
                Unknown      -> alertUser yellow sym "UNKNOWN"
                InProgress _ -> return ()

red, pink, green, yellow :: Color
red    = Red
pink   = Magenta
green  = Green
yellow = Yellow

alertUser :: Color -> String -> Text -> IO ()
alertUser c name result =
    let render = T.putStrLn . renderStrict . layoutPretty defaultLayoutOptions
        doc = pretty name <> ": " <> annotate (color c) (pretty result)
    in render doc

pickServer :: VerifyOpts -> P.Profile -> (String,Int)
pickServer vo p =
    fromMaybe (P.cisawHost p, P.cisawPort p)
              ((,) <$> cisawHost vo <*> cisawPort vo)

getBitcodeWithFiles :: VerifyOpts -> Maybe FilePath -> [FilePath] -> Config -> IO ByteString
getBitcodeWithFiles vo mf [] cfg = getBitcode vo mf cfg
getBitcodeWithFiles vo mf@(Just _) _ cfg = getBitcode vo mf cfg
getBitcodeWithFiles opts _ files _cfg =
 do warnOut opts "Building a bitcode from a file set."
    fp <- CawBuild.simpleBuildFiles files
    warnOut opts "Done building bitcode."
    BS.readFile fp `X.catch`
        (\(e::X.SomeException) ->
            do errOut opts $ "Error: " <> show e
               exitFailure)

getBitcode :: VerifyOpts -> Maybe FilePath -> Config -> IO ByteString
getBitcode opts bitfile cfg
  | Just file <- bitfile =
    let err = "Could not read user-specified bitcode file '" <> file <> "'"
    in readFileOrExit file err
  | otherwise =
     do warnOut opts "Bitcode file not specified. Building caw bitcode."
        fp <- CawBuild.buildBitcodeViaConfig cfg
        warnOut opts "Done building bitcode."
        BS.readFile fp `X.catch`
            (\(e::X.SomeException) ->
                do errOut opts $ "Error: " <> show e
                   exitFailure)
