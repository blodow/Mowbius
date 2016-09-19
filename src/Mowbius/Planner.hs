module Mowbius.Planner where

import Algebra.Clipper
import Data.Fixed
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Data.Vector

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
  mkEvent (pre, (v, a), post)
    | (v < pre) && (v < post) && a < 0 = InHull
    | (v < pre) && (v < post) = InHole
    | (v > pre) && (v > post) && a < 0 = OutHull
    | (v > pre) && (v > post) = OutHole
    | otherwise               = Middle

  visitVertices :: Polygon -> [Float] -> ((Float, (Float, Float), Float) -> a) -> [a]
  visitVertices _ [] _ = []
  visitVertices _ [_] _ = []
  visitVertices p ds f =
    map f $ zip3 (leftsOf ds) (zip ds angles) (rightsOf ds)

  path = toPath p
  angles = map angle pointTuples
  pointTuples = zip3 path (leftsOf path) (rightsOf path)
  angle ((ax,ay), (bx,by), (cx, cy)) = normalize $ (atan2 (ay-by) (ax-bx)) - (atan2 (cy-by) (cx-bx))
  normalize = ((-) pi) . (`mod'` (2 * pi)) . ((+) pi) -- normalize to [-pi:pi)

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop k $ cycle xs) xs
 where
   k | n >= 0 = length xs - n
     | n < 0  = -n

leftsOf, rightsOf :: [a] -> [a]
leftsOf = rotate (-1)
rightsOf = rotate 1


