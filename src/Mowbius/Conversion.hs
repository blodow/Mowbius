module Mowbius.Conversion
  ( mapT
  , translatePath
  , translatePoint
  , transformPath
  , transformPoint
  , floatToInt
  , intToFloat
  , pathsToPolys
  , pathToPoly
  , toPaths
  , toPath
  , toPoint
  ) where

import Algebra.Clipper
import GHC.Int
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle

-- Helper functions

mapT :: (a -> b) -> (a, a) -> (b, b)
mapT f (a1, a2) = (f a1, f a2)

translatePath :: Point -> Path -> Path
translatePath t = map $ translatePoint t

translatePoint :: Point -> Point -> Point
translatePoint (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

transformPath :: Float -> Point -> Path -> Path
transformPath r t = map $ transformPoint r t

transformPoint :: Float -> Point -> Point -> Point
transformPoint r t = translatePoint t . rotateV (-degToRad r)

floatToInt :: Float -> GHC.Int.Int64
floatToInt = round . (*) 100.0 -- 1 cm

intToFloat :: GHC.Int.Int64 -> Float
intToFloat i = (fromInteger $ toInteger i) / 100.0 -- 1 cm

pathsToPolys :: [Path] -> Polygons
pathsToPolys = Polygons . map pathToPoly

pathToPoly :: Path -> Polygon
pathToPoly p = 
  Algebra.Clipper.Polygon . flip map p $ \(x, y) -> IntPoint (floatToInt x) (floatToInt y)

toPaths :: Polygons -> [Path]
toPaths (Algebra.Clipper.Polygons ps) = map toPath ps

toPath :: Polygon -> Path
toPath (Algebra.Clipper.Polygon p) = map (\p' -> (x p', y p')) p
 where
  x = intToFloat . pointX
  y = intToFloat . pointY

toPoint :: IntPoint -> Point
toPoint p = mapT intToFloat (pointX p, pointY p)

