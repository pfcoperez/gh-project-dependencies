module Data.String.Utils (strip, findTaggedUrls, findAllUrls, filterWords)  where

import Data.Char
import qualified Data.Maybe as Maybe
import Network.URI as URI
import qualified Data.Map.Strict as Map

strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

filterWords :: [String] -> String -> String
filterWords filteredWords text = unwords $ filter (\x -> notElem x filteredWords) $ words text  

findTaggedUrls :: String -> Map.Map String [URI]
findTaggedUrls text =
    let ws = words $ Prelude.map toLower text
        entries =  zip ws $ Prelude.map URI.parseURI $ tail ws
    in Prelude.foldl (\ acc entry -> case entry of
                         (key, Just url) -> Map.insertWith (\ new old -> new ++ old) key [url] acc
                         _ -> acc
                     ) Map.empty entries

findAllUrls :: String -> [URI]
findAllUrls = (filter (\x -> (URI.uriScheme x) == "https:")) . (>>= Maybe.maybeToList . URI.parseURI) . words
