{-# LANGUAGE OverloadedStrings #-}

module Main where
import Data.Maybe

import DependenciesAssistant
import Data.Graph (graphToTree)
import Data.Set as Set
import Data.Tree as Tree
import Data.Char (isDigit, toLower)
import System.Environment (lookupEnv, getArgs)
import System.Exit (exitSuccess)

parseArgs :: [String] -> (Either String (String, String, Int, Int))
parseArgs [orgStr, repoStr, metaIssueStr, issueStr]
  | all (all isDigit) [metaIssueStr, issueStr] = Right (orgStr, repoStr, read metaIssueStr, read issueStr)
  | otherwise = Left "Organization, repository, metaissue and issue arguments are required"
parseArgs _ = Left "Invalid arguments list"

main :: IO ()
main = do
  maybeToken <- lookupEnv "TOKEN"
  _ <- if isJust maybeToken then
    return () else
    do
      _ <- putStrLn "GitHub token must be provided via TOKEN env var"
      exitSuccess
  let Just token = maybeToken

  strArgs <- getArgs

  res <- either return ( \ (organization, repository, metaIssue, issue) ->
                           do
                             graph <- metaIssueGraph token organization repository metaIssue
                             let tree = graphToTree issue Set.empty graph
                             let strTree = fmap (show) tree
                             _ <- renderIssuesTree "/tmp/deps.png" tree
                             return $ Tree.drawTree strTree
                       ) $ parseArgs strArgs  

  putStrLn res
