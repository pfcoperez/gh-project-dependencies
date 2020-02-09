module Data (
  StageState(..),
  StageIssue(..),
  module Data.Graphs,
  module Data.String.Utils
  ) where

import Data.Graphs
import Data.String.Utils

data StageState = Pending | InProgress | Done deriving Show

data StageIssue = StageIssue { issueNo :: Int
                             , metaIssueNo :: Maybe Int
                             , state :: StageState
                             } deriving (Show)
