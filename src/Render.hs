{-# LANGUAGE NoMonomorphismRestriction #-}
module Render (renderT, renderIssuesTree) where
  
import Diagrams.Backend.SVG
import Diagrams.Prelude
import Diagrams.TwoD.Layout.Tree
import Data.Tree
import Data (StageState(..), StageIssue(..))

layout :: Show a => Tree a -> Tree (a, P2 Float)
layout = symmLayout' (with & slHSep .~ 8 & slVSep .~ 8)

dimensions :: SizeSpec V2 Float
dimensions = dims2D 300 400

diagram :: Show a => Tree (a, P2 Float) -> QDiagram SVG V2 Float Any
diagram = pad 1.25 . centerXY .
  (renderTree (\n -> (text (show n) <> roundedRect 5 2 0.75 # fc lightgreen)) (~~))
             
renderT :: Show a => String -> Tree a -> IO ()
renderT outputPath tree = renderSVG outputPath dimensions (diagram $ layout tree)

issueColour :: StageIssue -> Colour Double
issueColour stageIssue = case (state stageIssue) of
                           Pending -> white
                           InProgress -> lightyellow
                           Done -> lightgreen

issueDiagram :: Tree (StageIssue, P2 Float) -> QDiagram SVG V2 Float Any
issueDiagram = pad 1.25 . centerXY .
  (renderTree (\issue -> (text (show $ issueNo issue) <> roundedRect 5 2 0.75 # fc (issueColour issue))) (~~))

renderIssuesTree :: String -> Tree StageIssue -> IO ()
renderIssuesTree outputPath tree = renderSVG outputPath dimensions (issueDiagram $ layout tree)
