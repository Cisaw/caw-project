{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ViewPatterns          #-}

module Caw.Types
    ( Status(..)
    , User(..), userToText, userFromText
    , Auth(..)
    , Response(..)
    , BitcodeLocation(..)
    , Task(..)
    , Ticket(..)
    , Outcome(..)
    , Progress(..)
    , NewJobInfo(..)
    , BitcodeDelivery(..)
    ) where

import           Data.List (intersperse)
import           Data.Aeson as A
import qualified Data.Aeson.Types as A
import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.Binary as Bin
import           Data.String
import           Data.Hashable
import           Data.Monoid
import qualified Data.Text as T
import           Data.Text (Text)
import           GHC.Generics
import           Servant.API.ContentTypes
import           Web.HttpApiData

-- * The CLI JSON API

-- | Tasks from stand-alone client (i.e. not Git{Hub,Lab} or other hosts with
-- their own web event schemas) 
--
-- The communication diagram is:
--
-- @
-- NewJob {}   -->
--             <-- JobStatus x Received
-- Bitcode _ x -->
--
-- and
--
-- PollJob x    -->
--             <-- JobStatus x _
-- @

type Symbol = Text

data NewJobInfo
    = NewPropJobInfo { testSymbol                 :: Symbol
                     , pureUninterpretedFunctions :: [Symbol]
                     -- , randomFunctions         :: [Symbol]
                     , noopFunctions              :: [Symbol]
                     , userAuthentication         :: Auth
                     , bitcode                    :: BitcodeLocation
                     }
    | NewEquivJobInfo { equivSymbol1, equivSymbol2 :: Symbol
                      , userAuthentication         :: Auth
                      , bitcode                    :: BitcodeLocation
                      }
    | NewDiffJobInfo { testSymbol                  :: Symbol
                     , userAuthentication          :: Auth
                     , bitcode                     :: BitcodeLocation
                     }
  deriving (Eq,Ord,Show,Generic)

data Task
    = NewJob  NewJobInfo
    | PollJob { ticketNumber              :: Ticket
              }
  deriving (Eq,Ord,Show,Generic)

data BitcodeLocation = BitcodeThere Text | BitcodePush
  deriving (Eq,Ord,Show,Generic)

data BitcodeDelivery
        = PropBitcode ByteString
        | EquivBitcode { eqBuild1, eqBuild2 :: ByteString }
        | DiffPropBitcode { dpBuild1, dpSpec, dpBuild2 :: ByteString}
    deriving (Eq,Ord,Show,Generic,Bin.Binary)

data Response
        = JobStatus { assignedTicket :: Ticket
                    , jobStatus      :: Status
                    }
  deriving (Eq,Ord,Show,Generic)

data Status
        = Other Text
        | ReceivedJob
        -- ^ New job, requires bitcode
        | ReceivedBitcode Outcome
        -- ^ Bitcode location received, processing or complete.
  deriving (Eq,Ord,Show,Generic)

-- | An authorized user of Cisaw
data User =
        User { userName          :: Text
             , userOAuthProvider :: Text -- no ':' allowed!
             }
    deriving (Eq,Ord,Show)

userToText :: User -> Text
userToText (User u p) = p <> ":" <> u
userFromText :: Text -> User
userFromText t | not (T.any (==':') t) = User t ""
               | otherwise = let (p,T.drop 1 -> u) = T.break (==':') t
                             in User u p


data Auth = Auth { authUser :: User, authToken :: Token }
  deriving (Eq,Ord,Show,Generic)

-- A 'Token' is akin to an account number allowing us to ensure the user is
-- valid.  The DogServer owner generates tokens, provides them to DogServer
-- users (people with repositories) and those users then enter the token in a
-- field in Git{Lab,Hub}.
type Token = Text


instance MimeRender OctetStream BitcodeDelivery where
    mimeRender _ = Bin.encode

instance MimeUnrender OctetStream BitcodeDelivery where
    mimeUnrender _ = either (Left . show) (\(_,_,x) -> Right x) . Bin.decodeOrFail

instance FromJSON Task where
    parseJSON (A.Object o) =
     do cmd <- o .: "cmd"
        case cmd of
            "newpropjob" ->
                        NewJob <$> (NewPropJobInfo <$> o .: "testSymbol"
                                                   <*> o .: "pureFunctions"
                                                   <*> o .: "noopFunctions"
                                                   <*> o .: "auth"
                                                   <*> o .: "bitcode" <|> fail "No 'bitcode' key in 'newpropjob' object."
                                                   )
            "newequivjob" ->
                        NewJob <$> (NewEquivJobInfo <$> o .: "symbol1"
                                                    <*> o .: "symbol2"
                                                    <*> o .: "auth"
                                                    <*> o .: "bitcode"
                                                   )
            "newdiffjob" ->
                        NewJob <$> (NewDiffJobInfo <$> o .: "testSymbol"
                                                   <*> o .: "auth"
                                                   <*> o .: "bitcode"
                                                   )
            "poll"   -> PollJob <$> o .: "ticket"
            x -> fail $ "Invalid 'cmd' for Task: '" ++ x ++ "'"
    parseJSON v = A.typeMismatch "Task" v

instance ToJSON Task where
   toJSON (NewJob (NewPropJobInfo {..})) =
    A.object [ "cmd"            .= ("newpropjob"::Text)
             , "testSymbol"     .= testSymbol
             , "pureFunctions"  .= pureUninterpretedFunctions
             , "noopFunctions"  .= noopFunctions
             , "bitcode"        .= bitcode
             , "auth"           .= userAuthentication
             ]
   toJSON (NewJob (NewEquivJobInfo {..})) =
    A.object [ "cmd"            .= ("newequivjob"::Text)
             , "symbol1"        .= equivSymbol1
             , "symbol2"        .= equivSymbol2
             , "auth"           .= userAuthentication
             , "bitcode"        .= bitcode
             ]
   toJSON (NewJob (NewDiffJobInfo {..})) =
    A.object [ "cmd"            .= ("newdiffjob"::Text)
             , "testSymbol"     .= testSymbol
             , "auth"           .= userAuthentication
             , "bitcode"        .= bitcode
             ]
   toJSON (PollJob {..}) = 
    A.object [ "cmd"            .= ("poll"::Text)
             , "ticket"         .= ticketNumber
             ]

instance FromJSON Response where
    parseJSON (A.Object o) =
        JobStatus <$> o .: "ticket"
                  <*> o .: "status"
    parseJSON v = A.typeMismatch "Response" v

instance ToJSON Response where
    toJSON (JobStatus t s) = A.object [ "ticket" .= t
                                      , "status" .= s
                                      ]

instance FromJSON Status where
    parseJSON (A.String "ReceivedJob") = pure ReceivedJob
    parseJSON (A.Object o) =
        do cmd <- o .: "status"
           case cmd of
            "Other"           -> Other <$> o .: "message"
            "ReceivedJob"     -> pure ReceivedJob
            "ReceivedBitcode" -> ReceivedBitcode <$> o .: "outcome"
            x -> fail $ "Invalid 'status' field for Status: '" ++ x ++ "'"
    parseJSON v = A.typeMismatch "Status" v
instance ToJSON Status where
    toJSON (Other o) = A.object [ "status" .= ("Other"::Text)
                                , "message" .= o
                                ]
    toJSON ReceivedJob = A.object [ "status" .= ("ReceivedJob"::Text)
                                  ]
    toJSON (ReceivedBitcode o) = A.object [ "status" .= ("ReceivedBitcode"::Text)
                                          , "outcome" .= o
                                          ]

instance FromJSON Outcome where
    parseJSON (Object o) =
        do c <- o .: "outcome" :: A.Parser Text
           case c of
            "failed"       -> Failed <$> o .: "counterexample"
            "quickchecked" -> pure QuickChecked
            "proven"       -> pure Proven
            "unknown"      -> pure Unknown
            "inprogress"   -> InProgress <$> o .: "progress"
            _              -> fail $ "Invalid 'outcome' field value: '" ++ T.unpack c ++ "'"
    parseJSON v = A.typeMismatch "Outcome" v
instance ToJSON Outcome where
    toJSON (Failed  ce)   = A.object [ "outcome"        .= ("failed"::Text)
                                     , "counterexample" .= ce]
    toJSON QuickChecked   = A.object [ "outcome" .= ("quickchecked"::Text)]
    toJSON Proven         = A.object [ "outcome" .= ("proven"::Text)]
    toJSON Unknown        = A.object [ "outcome" .= ("unknown"::Text)]
    toJSON (InProgress p) = A.object [ "outcome"  .= ("inprogress"::Text)
                                     , "progress" .= p]

instance FromJSON Progress where
    parseJSON (A.String "CompilingSpecification") = pure CompilingSpecification
    parseJSON (A.String "RunningProver") = pure RunningProver
    parseJSON v = A.typeMismatch "Progress" v

instance ToJSON Progress where
    toJSON CompilingSpecification = A.String "CompilingSpecification"
    toJSON RunningProver = A.String "RunningProver"

instance FromJSON BitcodeLocation where
    parseJSON (A.Object o) =
        do bc <- o .: "bitcode" <|> fail "No 'bitcode' key in BitcodeLocation object"
           case bc of
            "ref"  -> BitcodeThere <$> o .: "url"
            "push" -> pure BitcodePush
            _      -> fail $ "Invalid 'bitcode' field in BitcodeLocation: '" ++ bc ++ "'"
    parseJSON v = A.typeMismatch "BitcodeLocation" v

instance ToJSON BitcodeLocation where
    toJSON (BitcodeThere url) = A.object [ "bitcode" .= ("ref"::Text)
                                         , "url"     .= url
                                         ]
    toJSON BitcodePush = A.object ["bitcode" .= ("push"::Text)]

instance FromJSON Auth where
  parseJSON (A.Object o) =
    Auth <$> o .: "user"
         <*> o .: "token"
  parseJSON v = A.typeMismatch "Auth" v

instance ToJSON Auth where
  toJSON (Auth u t) = A.object [ "user" .= u, "token" .= t ]

instance ToJSON User where
  toJSON (User u p) = A.object [ "username" .= u, "oauthProvider" .= p ]

instance FromJSON User where
  parseJSON (A.Object o) =
    do u <- o .: "username"
       p <- o .: "oauthProvider"
       pure $ User u p
  parseJSON v = A.typeMismatch "User" v

instance FromJSON Ticket where
    parseJSON (A.String v) = pure $ Ticket v
    parseJSON v = A.typeMismatch "Ticket" v

instance ToJSON Ticket where
   toJSON (Ticket t) = A.String t

-- | A ticket is a unique (random) identifier of a spec.
data Ticket = Ticket Text
    deriving (Eq,Ord,Show,Read,Generic,Hashable)

instance IsString Ticket where
    fromString = Ticket . fromString

instance ToHttpApiData [Ticket] where
    toUrlPiece ts = T.concat (intersperse "," (map unticket ts))
      where unticket (Ticket t) = t

instance FromHttpApiData [Ticket] where
    parseUrlPiece = Right . decodeTicketString
      where decodeTicketString ts
              | T.length ts == 0 = []
              | otherwise =
                let (curr, rest) = T.span (/= ',') ts
                in Ticket curr : decodeTicketString (T.drop 1 rest)

instance ToHttpApiData User where
    toUrlPiece (User u p) = T.concat [p,":",u]
instance FromHttpApiData User where
    parseUrlPiece x =
       let (p,T.drop 1 -> u) = T.break (== ':') x
       in Right User { userName = u , userOAuthProvider = p}



data Outcome = Failed [Text]
             | QuickChecked
             | Proven
             | Unknown
             | InProgress Progress
  deriving (Eq, Ord, Show)

data Progress = CompilingSpecification | RunningProver
  deriving (Eq,Ord,Show,Enum)
