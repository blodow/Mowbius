module Mowbius.Planner where

import Algebra.Clipper
import Graphics.Gloss.Geometry.Angle

import Mowbius.Conversion

data VertexEvent = In
                 | Out
                 | Middle
                 deriving (Eq, Show)

decompose :: Polygon -> Float -> IO Polygons
decompose = undefined

eventsForDirection :: Float -> Polygon -> [VertexEvent]
eventsForDirection a p = markEvents $ distances a p

distances :: Float -> Polygon -> [Float]
distances angle (Polygon p) = map lineDistance p
 where
  lineDistance :: IntPoint -> Float
  lineDistance pt = let (x, y) = toPoint pt in
    ( a * x + b * y  + c ) / sqrt ( a * a + b * b ) 
  a = negate . sin $ degToRad angle
  b = cos $ degToRad angle
  c = 0.0

markEvents :: [Float] -> [VertexEvent]
markEvents d = visitVertices d mkEvent
 where
  mkEvent (pre, v, post)
    | (v > pre) && (v > post) = Out
    | (v < pre) && (v < post) = In
    | otherwise               = Middle

visitVertices :: [Float] -> ((Float, Float, Float) -> a) -> [a]
visitVertices [] _ = []
visitVertices [_] _ = []
visitVertices ds f = map f $ zip3 (rotate (-1) ds) ds (rotate 1 ds)

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop k $ cycle xs) xs
 where
   k | n >= 0 = length xs - n
     | n < 0  = -n



