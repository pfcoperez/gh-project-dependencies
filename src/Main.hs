{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.String.Utils
import qualified Data.Graph as Graph
import qualified GitHub.Endpoints.Issues as Issues
import GitHub.Data.Id (Id (Id))
import GitHub.Data (mkOwnerName, mkRepoName)
import Data.String (fromString)
import Data.Text (unpack)
import System.Environment (lookupEnv, getArgs)
import System.Exit (exitSuccess)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Network.URI as URI
import Data.Char (isDigit)
import Data.Text (pack)

maybeIssueId :: String -> String -> URI.URI -> Maybe Int
maybeIssueId organization repository uri = case (URI.pathSegments uri) of
                                             [org, repo, "issues", id] ->
                                               if org == organization && repo == repository then
                                                 Just ((read (filter isDigit id)) :: Int)
                                               else Nothing
                                             _ -> Nothing


metaIssueGraph :: String -> String -> String -> Int -> IO (Graph.Graph Int)
metaIssueGraph token organization repository issueId =
  let owner = mkOwnerName $ pack organization
      repo = mkRepoName $ pack repository
      fetchIssue issueId = Issues.issue' (Just $ Issues.OAuth $ fromString token) owner repo (Id issueId)
      discoverIO issueId = do
        errorOrIssue <- fetchIssue issueId
        let blockers issue =
              let tagged = (fmap (findTaggedUrls . unpack) . Issues.issueBody) issue
              in concat $ Maybe.maybeToList $ tagged >>= (\ m -> m Map.!? "by")
            foundBlockers = either (\_ -> []) blockers errorOrIssue
        return $ foundBlockers >>= (Maybe.maybeToList . (maybeIssueId organization repository))
      subIssues = do
        errorOrIssue <- fetchIssue issueId
        return $ either (\_ -> []) ( \x ->
                                       do
                                         body <- Maybe.maybeToList $ Issues.issueBody x
                                         url <- findAllUrls $ unpack body
                                         Maybe.maybeToList $ maybeIssueId organization repository url
                                   ) errorOrIssue
  in do
    seeds <- subIssues
    Graph.deepExploreIO discoverIO seeds Graph.empty
        

main :: IO ()
main = do
  maybeToken <- lookupEnv "TOKEN"
  _ <- if Maybe.isJust maybeToken then
    return () else
    do
      _ <- putStrLn "GitHub token must be provided via TOKEN env var"
      exitSuccess
  let Just token = maybeToken

  graph <- metaIssueGraph token "elastic" "cloud" 22175

  putStrLn $ show graph
  

--  errorOrIssue <- Issues.issue' (Just $ Issues.OAuth $ fromString token) "elastic" "cloud" (Id 22233)
--  putStrLn $ either
--    (\error -> "Error")
--    (show . fmap (findTaggedUrls . unpack) . Issues.issueBody) errorOrIssue
