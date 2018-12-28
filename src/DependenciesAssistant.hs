module DependenciesAssistant (metaIssueGraph, module Render) where
import qualified Network.URI as URI
import qualified Data.Graph as Graph
import qualified GitHub.Endpoints.Issues as Issues
import Data.Maybe
import GitHub.Data.Options (IssueState)
import GitHub.Data.Definitions (IssueLabel, labelName)
import Data (StageState(..), StageIssue(..))
import Data.String.Utils
import GitHub.Data.Id (Id (Id))
import GitHub.Data (mkOwnerName, mkRepoName)
import GitHub.Data.Name (untagName)
import Data.String (fromString)
import Data.Text (unpack)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Tree as Tree

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


metaIssueGraph :: String -> String -> String -> Int -> IO (Graph.Graph Int StageIssue)
metaIssueGraph token organization repository issueId =
  let owner = mkOwnerName $ pack organization
      repo = mkRepoName $ pack repository
      fetchIssue issueId = Issues.issue' (Just $ Issues.OAuth $ fromString token) owner repo (Id issueId)
      discoverIO issueId = do
        errorOrIssue <- fetchIssue issueId
        let issueInfo issue =
              let tagged = (fmap (findTaggedUrls . (filterWords ["by", "of"]) . (fmap (toLower)) . unpack) . Issues.issueBody) issue
                  blockers = concat $ maybeToList $ tagged >>= (\ m -> m Map.!? "blocked")
                  maybeMetaIssue = do
                    m <- tagged
                    metaIssues <- m Map.!? "part"
                    let ids = mapMaybe (maybeIssueId organization repository) metaIssues
                    listToMaybe ids
 
                  tagNames = fmap ((fmap toLower) . unpack . untagName . labelName) $ Issues.issueLabels issue
                  state = case (Issues.issueState issue, elem "in progress" tagNames) of
                            (Issues.StateClosed, _) -> Done
                            (_, True) -> InProgress
                            _ -> Pending
                            
              in (maybeMetaIssue, blockers, state)

            (foundMetaIssue, foundBlockers, foundState) = either (\_ -> (Nothing, [], Pending)) issueInfo errorOrIssue

        return $ (StageIssue {issueNo=issueId, metaIssueNo=foundMetaIssue, state=foundState}, mapMaybe (maybeIssueId organization repository) foundBlockers)

      subIssues = do
        errorOrIssue <- fetchIssue issueId
        return $ either (\_ -> []) ( \x ->
                                       do
                                         body <- maybeToList $ Issues.issueBody x
                                         url <- findAllUrls $ unpack body
                                         maybeToList $ maybeIssueId organization repository url
                                   ) errorOrIssue
  in do
    seeds <- subIssues
    Graph.deepExploreIO discoverIO seeds Graph.empty