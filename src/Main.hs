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
import qualified Data.Set as Set
import qualified Data.Tree as Tree
import qualified Data.Maybe as Maybe
import qualified Network.URI as URI
import Data.Char (isDigit, toLower)
import Data.Text (pack)
import Render

maybeIssueId :: String -> String -> URI.URI -> Maybe Int
maybeIssueId organization repository uri = case (URI.pathSegments uri) of
                                             [org, repo, "issues", id] ->
                                               if org == organization && repo == repository then
                                                 Just ((read (filter isDigit id)) :: Int)
                                               else Nothing
                                             _ -> Nothing

data StageState = Pending | InProgress | Done
data StageIssue = StageIssue { issueNo :: Int
                             , metaIssueNo :: Maybe Int
--                             , state :: StageState
                             } deriving (Show)


metaIssueGraph :: String -> String -> String -> Int -> IO (Graph.Graph Int StageIssue)
metaIssueGraph token organization repository issueId =
  let owner = mkOwnerName $ pack organization
      repo = mkRepoName $ pack repository
      fetchIssue issueId = Issues.issue' (Just $ Issues.OAuth $ fromString token) owner repo (Id issueId)
      discoverIO issueId = do
        errorOrIssue <- fetchIssue issueId
        let issueInfo issue =
              let tagged = (fmap (findTaggedUrls . (filterWords ["by", "of"]) . (fmap (toLower)) . unpack) . Issues.issueBody) issue
                  blockers = concat $ Maybe.maybeToList $ tagged >>= (\ m -> m Map.!? "blocked")
                  maybeMetaIssue = do
                    m <- tagged
                    metaIssues <- m Map.!? "part"
                    let ids = Maybe.mapMaybe (maybeIssueId organization repository) metaIssues
                    Maybe.listToMaybe ids
              in (maybeMetaIssue, blockers)

            (foundMetaIssue, foundBlockers) = either (\_ -> (Nothing, [])) issueInfo errorOrIssue

        return $ (StageIssue {issueNo=issueId, metaIssueNo=foundMetaIssue}, Maybe.mapMaybe (maybeIssueId organization repository) foundBlockers)

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

parseArgs :: [String] -> (Either String (String, String, Int, Int))
parseArgs [orgStr, repoStr, metaIssueStr, issueStr]
  | all (all isDigit) [metaIssueStr, issueStr] = Right (orgStr, repoStr, read metaIssueStr, read issueStr)
  | otherwise = Left "Organization, repository, metaissue and issue arguments are required"
parseArgs _ = Left "Invalid arguments list"

main :: IO ()
main = do
  maybeToken <- lookupEnv "TOKEN"
  _ <- if Maybe.isJust maybeToken then
    return () else
    do
      _ <- putStrLn "GitHub token must be provided via TOKEN env var"
      exitSuccess
  let Just token = maybeToken

  strArgs <- getArgs

  res <- either return ( \ (organization, repository, metaIssue, issue) ->
                           do
                             graph <- metaIssueGraph token organization repository metaIssue
                             let tree = fmap (issueNo) $ Graph.graphToTree issue Set.empty graph
                             _ <- renderT "/tmp/deps.svg" tree
                             return $ Tree.drawTree tree
                       ) $ parseArgs strArgs  

  putStrLn res
