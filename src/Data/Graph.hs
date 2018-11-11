--{-# LANGUAGE ScopedTypeVariables #-}

module Data.Graph (empty, addNode, addEdge, nodes, edges, deepExplore, deepExploreIO, graphToTree, Graph) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import Data.Tree as Tree

type Edges a = Set.Set a
type Graph a = Map.Map a (Edges a)

empty :: Ord a => Graph a
empty = Map.empty

addNode :: Ord a => a -> Graph a -> Graph a
addNode node graph = Map.insertWith (\ new old -> old ) node Set.empty graph

addEdge :: Ord a => a -> a -> Graph a -> Graph a
addEdge from to graph = Map.insertWith (\ new old -> Set.union new old) from (Set.singleton to) graph

nodes :: Graph a -> [a]
nodes = Map.keys

edges :: Ord a => a -> Graph a -> [a]
edges from graph = (Maybe.maybeToList (graph Map.!? from)) >>= Set.toList

deepExplore :: Ord a => (a -> [a]) -> [a] -> Graph a -> Graph a
deepExplore _ [] acc = acc
deepExplore discover (current:rem) acc =
  let adjacent = discover current
      updatedGraph = addNode current $ foldl ( \ graph node -> (addEdge current node) graph ) acc adjacent
  in deepExplore discover (filter (\candidate -> not $ Map.member candidate updatedGraph) $ adjacent ++ rem) updatedGraph

deepExploreIO :: Ord a => (a -> IO [a]) -> [a] -> Graph a -> IO (Graph a)
deepExploreIO _ [] acc = return acc
deepExploreIO discover (current:rem) acc = do
  adjacent <- discover current
  let updatedGraph = addNode current $ foldl ( \ graph node -> (addEdge current node) graph ) acc adjacent
  deepExploreIO discover (filter (\candidate -> not $ Map.member candidate updatedGraph) $ adjacent ++ rem) updatedGraph

graphToTree :: Ord a => a -> Set.Set a -> Graph a -> Tree a
graphToTree node visited graph
  | Set.member node visited = Tree.Node node []
  | otherwise = Tree.Node node $ fmap (\child -> graphToTree child (Set.insert node visited) graph) $ edges node graph
