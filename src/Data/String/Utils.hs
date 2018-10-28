module Data.String.Utils (strip)  where

import Data.Char
import Data.Maybe as Maybe
import Network.URI as URI
import qualified Data.Map.Strict as Map

strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse


findTaggedUrls :: String -> Map String URI
findTaggedUrls text =
    let ws = words $ Prelude.map toLower text
        entries =  zip ws $ Prelude.map URI.parseURI $ tail ws
        urls = Prelude.foldl (\ acc entry -> case entry of
                                 (key, Just url) -> (key, url) : acc
                                 _ -> acc
                             ) [] entries
    in fromList urls
