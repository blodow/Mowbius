module Mowbius.Planner where

import Algebra.Clipper
import Data.Fixed
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Text.Show.Pretty

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

data EdgeType = LeftEdge | RightEdge

data PointTag = Original Int
              | Created Int
              | NoTag
              deriving (Eq, Show)
type PolygonTag = Int

data TaggedPoint = TaggedPoint { tptTag :: PointTag
                               , tptDist :: Float
                               , tptPoint :: Point
                               } deriving (Show)

mkOrigPt :: Int -> Float -> Point -> TaggedPoint
mkOrigPt i d p = TaggedPoint (Original i) d p

data TaggedPolygon = TaggedPolygon { tpyTag :: !PolygonTag
                                   , tpyCell :: Cell
                                   , tpyPoints :: [Point]
                                   } deriving (Show)

type GraphEdge = (PolygonTag, PolygonTag)

instance Eq TaggedPolygon where
  TaggedPolygon tag _ _ == TaggedPolygon tag' _ _ = tag == tag'

instance Eq TaggedPoint where
  TaggedPoint tag _ _ == TaggedPoint tag' _ _ = tag == tag'

type Edge = (TaggedPoint, TaggedPoint)
type Edges = [Edge]

data Cell = Cell { ceIndex :: !PolygonTag
                 , ceLeft :: Edges
                 , ceRight :: Edges
                 } deriving (Show)
instance Eq Cell where
  Cell tag _ _ == Cell tag' _ _ = tag == tag'
type Cells = [Cell]

data Graph = Graph { grOrig :: [TaggedPoint]
                   , grCreated :: [TaggedPoint]
                   , grNodes :: [TaggedPolygon]
                   , grEdges :: [GraphEdge]
                   , crPolygonTag :: !PolygonTag
                   , crCreatedTag :: !Int
                   , grOpenCells :: Cells
                   } deriving (Show)

emptyGraph :: Polygon -> [Float] -> Graph
emptyGraph p ds = Graph (map (uncurry3 mkOrigPt) $ zip3 [0..] ds (toPath p)) [] [] [] 0 0 []
 where 
  uncurry3 :: (a->b->c->d) -> (a,b,c) -> d
  uncurry3 f (a,b,c) = f a b c

lookupEdge :: [TaggedPoint] -> [TaggedPoint] -> PointTag -> Edge
lookupEdge os os' (Original i) = (zip os os') !! i
lookupEdge os os' t = error ("origEdge: " ++ show os ++ " " ++ show os' ++ " " ++ show t)

rightEdge, leftEdge :: Graph -> PointTag -> Edge
rightEdge g = lookupEdge (grOrig g) (rightsOf $ grOrig g)
leftEdge g  = lookupEdge (leftsOf $ grOrig g) (grOrig g)

toNode :: Cell -> TaggedPolygon
toNode c@(Cell t' ls rs) = TaggedPolygon t' c $ dedup $
     (map tptPoint (collectPoints ls))
  ++ (map tptPoint (collectPoints $ reverse rs))
 where
  dedup :: Eq a => [a] -> [a]
  dedup [] = []
  dedup (x : (y : xs)) | x == y = x : xs
  dedup xs = xs
  collectPoints :: Edges -> [TaggedPoint]
  collectPoints = concatMap (\(p1,p2) -> [p1,p2])

splitEdge :: Float -> Int -> Edge -> (Edge, Edge)
splitEdge d i (p1@(TaggedPoint _ d1 _), p2@(TaggedPoint _ d2 _)) =
    let prop = (d - d1) / (d2 - d1)
        p = TaggedPoint (Created i) d $ (tptPoint p1) + (mulSV prop ((tptPoint p2) - (tptPoint p1)))
     in ((p1, p), (p, p2))

replaceEdge :: Edge -> Edge -> Edges -> Edges
replaceEdge old new es = map (\e -> if e == old then new else e) es

between :: Float -> Edge -> Bool
between d (TaggedPoint _ d1 _, TaggedPoint _ d2 _) = (d1 < d && d < d2) || (d1 > d && d > d2)

decompose :: Float -> Polygon -> Graph
decompose angle p@(Polygon ps) = foldl walk (emptyGraph p ds) sortedPoints
 where
  ds = distances angle p
  events = markEvents p ds
  sortedPoints = sortBy (comparing thrd) $ zip3 events tags ds
  tags = map Original [0..]
  thrd (_, _, c) = c

  inEdges sel pts t es = inAnyEdge sel es $ getPointWithTag pts t
  inAnyEdge sel es t' = any ((==) t' . sel) es
  inBothEdges g t (Cell _ es1 es2) = (inEdges fst (grOrig g) t es1) && (inEdges snd (grOrig g) t es2)
  inLeftEdges g t (Cell _ ls _) = inEdges fst (grOrig g) t ls
  inRightEdges g t (Cell _ _ rs) = inEdges snd (grOrig g) t rs

  getPointWithTag :: [TaggedPoint] -> PointTag -> TaggedPoint
  getPointWithTag pts t = maybe (TaggedPoint NoTag 0 (-1, -1)) id $ find (hasTag t) pts

  hasTag :: PointTag -> TaggedPoint -> Bool
  hasTag t1 (TaggedPoint t2 _ _) = t1 == t2
  hasTag _ _ = False

  walk :: Graph -> (VertexEvent, PointTag, Float) -> Graph
  walk g (InHull, t, d) = create g t
  walk g (InHole, t, d) = split g t d
  walk g (OutHull, t, d) = close g t
  walk g (OutHole, t, d) = join g t d
  walk g (Middle, t, d) = update g t

  create, update, close
    :: Graph -> PointTag -> Graph

  --------------------------------------------------------------------------------------------------------------- create
  create g t = let pTag = crPolygonTag g + 1
                in g { grOpenCells = Cell pTag [leftEdge g t] [rightEdge g t] : grOpenCells g
                     , crPolygonTag = pTag }

  --------------------------------------------------------------------------------------------------------------- update
  update g t = g { grOpenCells = map upd (grOpenCells g) }
   where
    upd :: Cell -> Cell
    upd c@Cell { ceRight = r } | inEdges snd (grOrig g) t r = c { ceRight = rightEdge g t : r }
    upd c@Cell { ceLeft = l } | inEdges fst (grOrig g) t l = c { ceLeft = leftEdge g t : l }
    upd c = c

  ---------------------------------------------------------------------------------------------------------------- close
  close g t = let c = maybe (Cell 666 [] []) id $ find (inBothEdges g t) (grOpenCells g)
               in g { grOpenCells = delete c (grOpenCells g)
                    , grNodes = toNode c : grNodes g
                    }

  ---------------------------------------------------------------------------------------------------------------- split
  split :: Graph -> PointTag -> Float -> Graph
  split g t d = g { grOpenCells = new1 : new2 : (delete old $ grOpenCells g)
                  , crPolygonTag = pTag2
                  , crCreatedTag = cTag2
                  , grNodes = toNode old : grNodes g }
   where
    pTag1 = crPolygonTag g + 1
    pTag2 = crPolygonTag g + 2
    cTag1 = crCreatedTag g + 1
    cTag2 = crCreatedTag g + 2
    (new1, new2, old) = case catMaybes $ map betweenEdges $ grOpenCells g of
                          [(cell, r, l)] -> split' cell r l
                          sthElse -> error ("Split:\n" ++ ppShow g ++ "\n\n" ++ ppShow sthElse)

    betweenEdges :: Cell -> Maybe (Cell, Edge, Edge)
    betweenEdges c = let rs = filter (between d) (ceRight c)
                         ls = filter (between d) (ceLeft c)
                      in case (rs, ls) of
                         ([r], [l]) -> if insideCell r l pt' then Just (c, r, l) else Nothing
                         otherwise -> Nothing

    pt' = getPointWithTag (grOrig g) t
    insideCell :: Edge -> Edge -> TaggedPoint -> Bool
    insideCell e1 e2 p = leftOfEdge e1 p && leftOfEdge e2 p
    leftOfEdge (TaggedPoint _ _ (x1,y1), TaggedPoint _ _ (x2,y2)) (TaggedPoint _ _ (x,y)) = 
      let x' = x - x1
          y' = y - y1
          x2' = x2 - x1
          y2' = y2 - y1
       in x'*y2' - x2'*y' > 0

    split' :: Cell -> Edge -> Edge -> (Cell, Cell, Cell)
    split' c r l = let (edgeLFront, edgeLBack) = splitEdge d cTag1 l
                       (edgeRFront, edgeRBack) = splitEdge d cTag2 r
                    in ( Cell pTag1 [edgeLFront] [rightEdge g t]
                       , Cell pTag2 [leftEdge g t] [edgeRBack]
                       , c { ceLeft = replaceEdge l edgeLBack (ceLeft c)
                           , ceRight = replaceEdge r edgeRFront (ceRight c)
                           }
                       )

  ----------------------------------------------------------------------------------------------------------------- join
  join :: Graph -> PointTag -> Float -> Graph
  join g t d = g { grOpenCells = new : (grOpenCells g \\ [old1, old2])
                 , crPolygonTag = pTag
                 , crCreatedTag = cTag2
                 , grNodes = toNode old1 : toNode old2 : grNodes g }
   where
    pTag = crPolygonTag g + 1
    cTag1 = crCreatedTag g + 1
    cTag2 = crCreatedTag g + 2

    (old1, old2, new) = let rs = filter (inLeftEdges g t) (grOpenCells g)
                            ls = filter (inRightEdges g t) (grOpenCells g)
                         in case (rs, ls) of
                              ([r], [l]) -> join' r l
                              sthElse -> error ("Join:\n" ++ ppShow g ++ "\n\n" ++ ppShow sthElse)

    join' :: Cell -> Cell -> (Cell, Cell, Cell)
    join' r l = let edgeL = selectE $ ceLeft l
                    edgeR = selectE $ ceRight r
                    (edgeLFront, edgeLBack) = splitEdge d cTag1 edgeL
                    (edgeRFront, edgeRBack) = splitEdge d cTag2 edgeR
                 in ( l { ceLeft = replaceEdge edgeL edgeLBack (ceLeft l) }
                    , r { ceRight = replaceEdge edgeR edgeRFront (ceRight r) }
                    , Cell pTag [edgeLFront] [edgeRBack]
                    )

    selectE :: Edges -> Edge
    selectE es = maybe emptyEdge id $ find (between d) es
   
    emptyEdge :: Edge
    emptyEdge = (emptyPt, emptyPt)
    emptyPt = TaggedPoint NoTag 0 (0,0)


