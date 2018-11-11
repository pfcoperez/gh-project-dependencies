--{-# LANGUAGE ScopedTypeVariables #-}

module Data.Graph (empty, addNode, addEdge, nodes, edges, deepExplore, deepExploreIO, graphToTree, Graph) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import Data.Tree as Tree

type Edges k = Set.Set k
type Graph k v = Map.Map k (v, (Edges k))

empty :: Ord k => Graph k v
empty = Map.empty

addNode :: Ord k => (k, v) -> Graph k v -> Graph k v
addNode (k, v) graph = Map.insertWith (\ new old -> old ) k (v, Set.empty) graph

addEdge :: Ord k => k -> k -> Graph k v -> Graph k v
addEdge from to graph = Map.update (\ (oldV, oldEdges) -> Just (oldV, Set.insert to oldEdges)) from graph

nodes :: Graph k v -> [k]
nodes = Map.keys

edges :: Ord k => k -> Graph k v -> [k]
edges from graph = (Maybe.maybeToList (graph Map.!? from)) >>= (Set.toList . snd)

nodeValue :: Ord k => k -> Graph k v -> v
nodeValue node graph = let (v, _) = (graph Map.! node) in v

deepExplore :: Ord k => (k -> [k]) -> (k -> v) -> [k] -> Graph k v -> Graph k v
deepExplore _ _ [] acc = acc
deepExplore discover details (current:rem) acc =
  let adjacent = discover current
      value = details current
      updatedGraph = addNode (current, value) $ foldl ( \ graph node -> (addEdge current node) graph ) acc adjacent
  in deepExplore discover details (filter (\candidate -> not $ Map.member candidate updatedGraph) $ adjacent ++ rem) updatedGraph

deepExploreIO :: (Ord k, Monad m) => (k -> m [k]) -> (k -> m v) -> [k] -> Graph k v -> m (Graph k v)
deepExploreIO _ _ [] acc = return acc
deepExploreIO discover details (current:rem) acc = do
  adjacent <- discover current
  value <- details current
  let updatedGraph = addNode (current, value) $ foldl ( \ graph node -> (addEdge current node) graph ) acc adjacent
  deepExploreIO discover details (filter (\candidate -> not $ Map.member candidate updatedGraph) $ adjacent ++ rem) updatedGraph

graphToTree :: Ord k => k -> Set.Set k -> Graph k v -> Tree v
graphToTree node visited graph
  | Set.member node visited = Tree.Node (nodeValue node graph) []
  | otherwise = Tree.Node (nodeValue node graph) $ fmap (\child -> graphToTree child (Set.insert node visited) graph) $ edges node graph
