{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.String.Utils
import qualified GitHub.Endpoints.Issues as Issues
import GitHub.Data.Id (Id (Id))
import Data.String (fromString)
import Data.Text (unpack)

main :: IO ()
main = do
  errorOrIssue <- Issues.issue' (Just $ Issues.OAuth $ fromString "SECRET") "elastic" "cloud" (Id 22233)
  putStrLn $ either
    (\error -> "Error")
--    (show . Issues.issueTitle)
--    (show . Issues.issueBody)
    (show . fmap (findTaggedUrls . unpack) . Issues.issueBody)
    errorOrIssue
