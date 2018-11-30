{-# LANGUAGE NoMonomorphismRestriction #-}
module Render (renderT) where
  
import Diagrams.Backend.SVG
import Diagrams.Prelude
import Diagrams.TwoD.Layout.Tree
import Data.Tree 

layout :: Show a => Tree a -> Tree (a, P2 Float)
layout = symmLayout' (with & slHSep .~ 8 & slVSep .~ 8)

dimensions :: SizeSpec V2 Float
dimensions = dims2D 400 400

diagram :: Show a => Tree (a, P2 Float) -> QDiagram SVG V2 Float Any
diagram = pad 1.25 . centerXY .
  (renderTree (\n -> (text (show n) <> roundedRect 5 2 0.75 # fc lightgreen)) (~~))
             
renderT :: Show a => String -> Tree a -> IO ()
renderT outputPath tree = renderSVG outputPath dimensions (diagram $ layout tree)



