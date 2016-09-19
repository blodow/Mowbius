module Mowbius.Planner where

import Algebra.Clipper
import Data.Fixed
import Data.List
import Data.Ord
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
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

type Tag = Int
type Edge = (Tag, Tag)
type PointTag = Tag
data TaggedPolygon = TaggedPolygon Tag [PointTag]

instance Eq TaggedPolygon where
  TaggedPolygon tag _ == TaggedPolygon tag' _ = tag == tag'

data Graph = Graph Polygon [TaggedPolygon] [Edge]

edges :: Graph -> [Edge]
edges (Graph _ _ es) = es

nodes :: Graph -> [TaggedPolygon]
nodes (Graph _ ns _) = ns

data Cell = Cell { index :: Int
                 , left :: [(Point, Point)]
                 , right :: [(Point, Point)]
                 }

type Cells = [Cell]

splitPolygon :: Float -> Polygon -> Graph
splitPolygon angle p@(Polygon ps) = snd $ foldl f ([], empty) sorted
 where
  empty = Graph p [] []

  ds :: [Float]
  ds = distances angle p

  events :: [VertexEvent]
  events = markEvents p ds

  protoNode :: [(PointTag, VertexEvent)]
  protoNode = zip [0..] events

  sorted :: [(PointTag, VertexEvent)]
  sorted = sortBy (comparing snd) protoNode
  
  f :: (Cells, Graph) -> (PointTag, VertexEvent) -> (Cells, Graph)
  f (cs,_) (t, InHull) = create t : cs
  f (cs,_) (t, InHole) = split cs
  f (cs,_) (_, OutHull) = close cs
  f (cs,_) (_, OutHole) = join cs

  create cs = (cs, empty)
  join _ = (undefined, empty)
  close _ = (undefined, empty)
  split _ = (undefined, empty)




--
--splitPolygon :: Float -> Polygon -> Graph
--splitPolygon angle p@(Polygon ps) = foldl f empty sorted
-- where
--  empty = Graph p [] []
--
--  ds :: [Float]
--  ds = distances angle p
--
--  events :: [VertexEvent]
--  events = markEvents ds
--
--  protoNode :: [(PointTag, Float, VertexEvent)]
--  protoNode = zip3 [0..] ds events
--
--  sorted :: [(PointTag, Float, VertexEvent)]
--  sorted = sortBy (comparing snd3) protoNode
--
--  f :: Graph -> (PointTag, Float, VertexEvent) -> Graph
--  f empty (t, _, ev) = Graph p [TaggedPolygon 0 [t]] []
--  f g (t, _, Middle) = addT g t
--  f g (t, _, In) =  newT g t
--  f g (t, _, Out) = addT g t
--
--  newT :: Graph -> PointTag -> Graph
--  newT g _ = g
--
--  addT :: Graph -> PointTag -> Graph
--  addT (Graph p (TaggedPolygon i ts : tss) es) t = Graph p ((TaggedPolygon i (t : ts)) : tss) es
--
--  snd3 :: (a, b, c) -> b
--  snd3 (_, b, _) = b
--
graphToPolys :: Graph -> Polygons
graphToPolys (Graph p ps _) = Polygons $ map (untag p) ps

untag :: Polygon -> TaggedPolygon -> Polygon
untag (Polygon points) (TaggedPolygon _ indices) = Polygon $ map ((!!) points) indices

--cut :: Float -> Float -> Polygon
--cut angle distance p = undefined


