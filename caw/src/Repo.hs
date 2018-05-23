module Repo
    ( checkout
    ) where

import           Data.Text as T
import           System.Process

type CommitID = Text

checkout :: repoOptionsSomeDay -> CommitID -> IO ()
checkout _ cid = gitcheckout cid

-- | Checkout out a given commit.
gitcheckout :: Text -> IO ()
gitcheckout commitNr = callProcess "git" ["checkout", T.unpack commitNr]
