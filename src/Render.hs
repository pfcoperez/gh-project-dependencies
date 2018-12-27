{-# LANGUAGE NoMonomorphismRestriction #-}
module Render (renderT, renderIssuesTree) where
  
import Diagrams.Backend.Cairo
import Diagrams.Prelude
import Diagrams.TwoD.Layout.Tree
import Data.Tree
import Data (StageState(..), StageIssue(..))

layout :: Show a => Tree a -> Tree (a, P2 Double)
layout = symmLayout' (with & slHSep .~ 8 & slVSep .~ 8)

dimensions :: SizeSpec V2 Double
dimensions = dims2D 300 400

diagram :: Show a => Tree (a, P2 Double) -> QDiagram Cairo V2 Double Any
diagram = pad 1.25 . centerXY .
  (renderTree (\n -> (text (show n) <> roundedRect 5 2 0.75 # fc lightgreen)) (~~))
             
renderT :: Show a => String -> Tree a -> IO ()
renderT outputPath tree = renderCairo outputPath dimensions (diagram $ layout tree)

issueColour :: StageIssue -> Colour Double
issueColour stageIssue = case (state stageIssue) of
                           Pending -> white
                           InProgress -> lightyellow
                           Done -> lightgreen

renderDependency :: P2 Double -> P2 Double -> QDiagram Cairo V2 Double Any
renderDependency = arrowBetween' (with & arrowHead .~ dart & headGap .~ 20.0)

issueDiagram :: Tree (StageIssue, P2 Double) -> QDiagram Cairo V2 Double Any
issueDiagram = pad 1.25 . centerXY .
  (renderTree (\issue -> (text (show $ issueNo issue) <> roundedRect 5 2 0.75 # fc (issueColour issue))) renderDependency)

renderIssuesTree :: String -> Tree StageIssue -> IO ()
renderIssuesTree outputPath tree = renderCairo outputPath dimensions (issueDiagram $ layout tree)
