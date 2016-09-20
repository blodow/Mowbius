module Mowbius.Planner where

import Algebra.Clipper
import Data.Fixed
import Data.List
import Data.Maybe
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

eventsForDirection :: Float -> Polygon -> [VertexEvent]
eventsForDirection a p = markEvents p $ distances a p

distances :: Float -> Polygon -> [Float]
distances angle (Polygon p) = map lineDistance p
 where
  lineDistance :: IntPoint -> Float
  lineDistance pt = let (x, y) = toPoint pt in
    ( a * x + b * y + c ) / sqrt ( a * a + b * b ) 
  a = negate . sin $ degToRad angle
  b = cos $ degToRad angle
  c = 0.0

markEvents :: Polygon -> [Float] -> [VertexEvent]
markEvents p d = visitVertices p d mkEvent
 where
  mkEvent (pre, (v, a), post)
    | (v < pre) && (v < post) = if a < 0 then InHull  else InHole
    | (v > pre) && (v > post) = if a < 0 then OutHull else OutHole
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

-- #############################################################################

type PointTag = Int
type PolygonTag = Int
type NodeTag = Int

data TaggedPoint = TaggedPoint { tptTag :: Maybe PointTag
                               , tptDist :: Float
                               , tptPoint :: Point
                               }

mkPt :: PointTag -> Float -> Point -> TaggedPoint
mkPt t d p = TaggedPoint (Just t) d p

data TaggedPolygon = TaggedPolygon { tpyTag :: !PolygonTag
                                   , tpyPoints :: [Point]
                                   }

type GraphEdge = (PolygonTag, PolygonTag)

instance Eq TaggedPolygon where
  TaggedPolygon tag _ == TaggedPolygon tag' _ = tag == tag'

instance Eq TaggedPoint where
  TaggedPoint tag _ _ == TaggedPoint tag' _ _ = tag == tag'

data Cell = Cell { ceIndex :: !PolygonTag
                 , ceLeft :: [(Point, Point)]
                 , ceRight :: [(Point, Point)]
                 }
instance Eq Cell where
  Cell tag _ _ == Cell tag' _ _ = tag == tag'
type Cells = [Cell]

data Graph = Graph { grOrig :: [TaggedPoint]
                   , grNodes :: [TaggedPolygon]
                   , grEdges :: [GraphEdge]
                   , grCurTag :: !PolygonTag
                   , grOpenCells :: Cells
                   }
emptyGraph :: Polygon -> [Float] -> Graph
emptyGraph p ds = Graph (map (uncurry3 mkPt) $ zip3 [0..] ds (toPath p)) [] [] 0 []

uncurry3 :: (a->b->c->d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

leftEdgeP :: [Point] -> PointTag -> (Point, Point)
leftEdgeP p t = (zip p (leftsOf p)) !! t

rightEdgeP :: [Point] -> PointTag -> (Point, Point)
rightEdgeP p t = (zip p (rightsOf p)) !! t

decompose :: Float -> Polygon -> Graph
decompose angle p@(Polygon ps) = foldl walk (emptyGraph p ds) sortedPoints
 where
  ds = distances angle p
  events = markEvents p ds
  sortedPoints = sortBy (comparing thrd) $ zip3 events [0..] ds
  thrd (_, _, c) = c
  -- dropThird (a, b, _) = (a, b)

  leftEdge = leftEdgeP $ toPath p
  rightEdge = rightEdgeP $ toPath p

  inEdges :: [TaggedPoint] -> PointTag -> [(Point, Point)] -> Bool
  inEdges pts t es = let t' = getPointWithTag pts t in any (cmp t' . snd) es
  cmp :: TaggedPoint -> Point -> Bool
  cmp (TaggedPoint _ _ p1) p2 = abs (magV (p2-p1)) < 0.001
  inBothEdges g t (Cell _ es1 es2) = (inEdges (grOrig g) t es1) && (inEdges (grOrig g) t es2)
  inEitherEdges g t (Cell _ es1 es2) = (inEdges (grOrig g) t es1) || (inEdges (grOrig g) t es2)
  inLeftEdges g t (Cell _ ls _) = inEdges (grOrig g) t ls
  inRightEdges g t (Cell _ _ rs) = inEdges (grOrig g) t rs

  getPointWithTag :: [TaggedPoint] -> PointTag -> TaggedPoint
  getPointWithTag pts t = case dropWhile (not . sameTag t) pts of
               [] -> TaggedPoint Nothing 0 (0,0)
               l  -> head l
  sameTag :: PointTag -> TaggedPoint -> Bool
  sameTag t1 (TaggedPoint (Just t2) _ _) = t1 == t2
  sameTag _ _ = True

  walk :: Graph -> (VertexEvent, PointTag, Float) -> Graph
  walk g (InHull, t, d) = create g t
  walk g (InHole, t, d) = split g t
  walk g (OutHull, t, d) = close g t
  walk g (OutHole, t, d) = join g t d
  walk g (Middle, t, d) = update g t

  create, update, close, split
    :: Graph -> PointTag -> Graph

  create g t = let tag = grCurTag g + 1 in
    g { grOpenCells = Cell tag [leftEdge t] [rightEdge t] : grOpenCells g
      , grCurTag = tag }

  update g t = g { grOpenCells = map upd (grOpenCells g) }
   where
    upd :: Cell -> Cell
    upd c@Cell { ceLeft  = l } | inEs t l = c { ceLeft  = leftEdge  t : l }
    upd c@Cell { ceRight = r } | inEs t r = c { ceRight = rightEdge t : r }
    upd c = c
    inEs = inEdges (grOrig g)

  close g t = g { grOpenCells = delete c (grOpenCells g)
                , grNodes = toNode c pt' : grNodes g
                }
   where
    c = case filter (inBothEdges g t) (grOpenCells g) of
          [] -> Cell 666 [] [] 
          l  -> head l
    pt' = getPointWithTag (grOrig g) t

  toNode :: Cell -> TaggedPoint -> TaggedPolygon
  toNode (Cell t' ls rs) (TaggedPoint _ _ p) = TaggedPolygon t' $ p :
    (map fst ls) ++ (map fst (reverse rs))

  split g _ = g

  join :: Graph -> PointTag -> Float -> Graph
  join g t d = g { grOpenCells = c : (grOpenCells g \\ [a, b])
                 , grCurTag = tag
                 , grNodes = toNode a pt' : toNode b pt' : grNodes g }
   where

    (a, b, c) = let ls = filter (inLeftEdges g t) (grOpenCells g)
                    rs = filter (inRightEdges g t) (grOpenCells g) in
                    case (ls, rs) of
                      ([l], [r]) -> doit l r
                      otherwise -> undefined
    tag = grCurTag g + 1

    doit :: Cell -> Cell -> (Cell, Cell, Cell)
    doit l r = let edgeL = selectE $ ceLeft r
                   edgeR = selectE $ ceRight l
                   (edgeLFront, edgeLBack) = splitEdge d edgeL
                   (edgeRFront, edgeRBack) = splitEdge d edgeR in
                  ( r { ceLeft = replaceE edgeL edgeLFront (ceLeft r) }
                  , l { ceRight = replaceE edgeR edgeRFront (ceRight l) }
                  , Cell tag [edgeLBack] [edgeRBack]
                  )
    replaceE old new = map $ \e -> if e == old then new else e
    --selectE :: [(Point, Point)] -> (Point, Point)
    selectE es = case filter (cmp pt' . snd) es of
                   [] -> ((0,0),(0,0))
                   l -> head l
    splitEdge :: Float -> (Point, Point) -> ((Point, Point), (Point, Point))
    splitEdge _ e = (e, e)

    pt' = getPointWithTag (grOrig g) t



