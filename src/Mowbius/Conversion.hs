module Mowbius.Conversion
  ( mapT
  , translatePath
  , translatePoint
  , transformPath
  , transformPoint
  , floatToInt
  , intToFloat
  ) where

import GHC.Int
import Graphics.Gloss
import Graphics.Gloss.Data.Vector

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

