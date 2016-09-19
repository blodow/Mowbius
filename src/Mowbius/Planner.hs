module Mowbius.Planner where

import Algebra.Clipper
import Graphics.Gloss.Geometry.Angle

import Mowbius.Conversion

data VertexEvent = InHull
                 | InHole
                 | OutHull
                 | OutHole
                 | Middle
                 deriving (Eq, Show)

decompose :: Polygon -> Float -> IO Polygons
decompose = undefined

eventsForDirection :: Float -> Polygon -> [VertexEvent]
eventsForDirection a p = markEvents p $ distances a p

distances :: Float -> Polygon -> [Float]
distances angle (Polygon p) = map lineDistance p
 where
  lineDistance :: IntPoint -> Float
  lineDistance pt = let (x, y) = toPoint pt in
    ( a * x + b * y  + c ) / sqrt ( a * a + b * b ) 
  a = negate . sin $ degToRad angle
  b = cos $ degToRad angle
  c = 0.0

markEvents :: Polygon -> [Float] -> [VertexEvent]
markEvents p d = visitVertices p d mkEvent
 where
  mkEvent ((pre, apre), v, (post, apost))
    | (v < pre) && (v < post) && apost < apre = InHull
    | (v < pre) && (v < post) = InHole
    | (v > pre) && (v > post) && apost < apre = OutHull
    | (v > pre) && (v > post) = OutHole
    | otherwise               = Middle

  visitVertices :: Polygon -> [Float] -> (((Float, Float), Float, (Float, Float)) -> a) -> [a]
  visitVertices _ [] _ = []
  visitVertices _ [_] _ = []
  visitVertices p ds f =
    map f $ zip3 (zip (leftsOf ds) apres) ds (zip (rightsOf ds) aposts)

  path = toPath p
  apres = map angle . zip path $ leftsOf path
  aposts = map angle . zip path $ rightsOf path
  angle ((x1,y1), (x2,y2)) = atan2 (y2-y1) (x2-x1)

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop k $ cycle xs) xs
 where
   k | n >= 0 = length xs - n
     | n < 0  = -n

leftsOf, rightsOf :: [a] -> [a]
leftsOf = rotate (-1)
rightsOf = rotate 1


