module Mowbius (run) where

import Algebra.Clipper
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle

import Mowbius.Conversion
import Mowbius.Types

run :: IO ()
run = wInit >>= \wInit' -> 
  playIO (InWindow "uMow" windowSize (5, 5)) (greyN 0.2)  -- create gray window
             60 -- fps
             wInit' -- initial world state
             (return . displayWorld) -- display function
             handleEventIO -- event handler
             advanceIO -- state update function

-- constants

windowSize :: (Int, Int)
windowSize = (800, 600)

wInit :: IO World
wInit = clipHoles f >>= \f' -> return $ World f' b none
 where
  -- Field
  f = Field [p1, p2] [h]
  p1 = [(-5.0, -5.0), (5.0, -5.0), (5.0, 5.0), (-5.0, 5.0)]
  p2 = translatePath (11,0) p1
  h = [(1.0, 1.0), (4.0, 4.0), (4.0, 1.0)]
  -- Bot
  b = Bot (0.0, 0.0) 0.0 [] 0.3
  -- Keys
  none = Keys False False False False

clipHoles :: Field -> IO Field
clipHoles (Field f h) = toPaths <$> clip f' h'
                          >>= \f'' -> return $ Field f'' h
 where
  clip = execute ctDifference 
  f' = pathsToPolys f
  h' = pathsToPolys h

speed :: Float
speed = 0.05

rotSpeed :: Float
rotSpeed = 1
              
-- functions

handleEventIO :: Event -> World -> IO World
handleEventIO (EventKey (SpecialKey k) Down _ _) w = return w { keys = enable k $ keys w }
handleEventIO (EventKey (SpecialKey k) Up _ _) w = return w { keys = disable k $ keys w }
handleEventIO _ w = return w

advanceIO :: Float -> World -> IO World
advanceIO _ w@(World f b k) = case (s, r) of
 (0, 0) -> return w
 _ -> do
   f' <- mow f b
   return w { field = f', bot = go s r b }
 where
  step (Keys True False _ _) = speed -- forwards
  step (Keys False True _ _) = -speed -- backwards
  step _ = 0
  s = step k
  rot (Keys _ _ True False) = -rotSpeed -- left
  rot (Keys _ _ False True) = rotSpeed -- right
  rot _ = 0
  r = rot k

-- move robot by rotating r and taking a step with magnitude t
go :: Float -> Float -> Bot -> Bot
go t r b = b { pos = p, angle = r', path = take 200 $ p : path b}
 where
  r' = r + angle b
  p = translatePoint step (pos b)
  step = rotateV (-degToRad r') (0, t)

-- Display functions

displayWorld :: World -> Picture
displayWorld w@(World f b _) = autoScaled w $ pictures [ displayField f, displayBot b ]

autoScaled :: World -> Picture -> Picture
autoScaled w = Translate tx ty . Scale s s 
 where
  s = 0.9 * minimum [sx, sy]
  sx = ((fromInteger . toInteger . fst) windowSize) / width worldBounds
  sy = ((fromInteger . toInteger . snd) windowSize) / height worldBounds
  worldBounds = getBounds w
  center (p, q) = (mulSV 0.5 (q - p)) + p  -- compute center between two points
  c = center $ mapT (mulSV s) worldBounds -- scale worldbounds and get center
  tx = - fst c
  ty = - snd c


  width :: (Point, Point) -> Float
  width (p, q) = fst (q - p)
  height :: (Point, Point) -> Float
  height (p, q) = snd (q - p)

displayField :: Field -> Picture
displayField Field {fields = fs, holes = hs} = pictures [fs', hs']
 where
  fs' = Color green . pictures $ map lineLoop fs
  hs' = Color red . pictures $ map lineLoop hs

displayBot :: Bot -> Picture
displayBot b = pictures [path', t $ r botp]
 where
  path' = Color yellow . line $ path b
  t = Translate x y
  r = Rotate $ angle b
  x = fst $ pos b
  y = snd $ pos b
  w = (wheelBase b) / 2.0
  h = w / 3.0
  botp = Pictures [ Color blue $ thickCircle 0.1 w
                  , Color white $ rectangleSolid w h
                  , Color white $ line [(0, 0), (0, 10*w)]
                  ]

getBounds :: World -> (Point, Point)
getBounds w = ((minimum xs, minimum ys), (maximum xs, maximum ys))
 where
  -- allPoints contains robot pose and all field vertices
  allPoints = pos (bot w) : (concat . fields $ field w)
  (xs, ys) = unzip allPoints

-- Mowing

mow :: Field -> Bot -> IO Field
mow f b = toPaths <$> clip ps bot' 
                  >>= \f' -> return f { fields = f' }
 where
  clip = execute ctDifference 
  ps = pathsToPolys $ fields f
  bot' = Polygons $ [pathToPoly botFootPrint]
  botFootPrint = transformPath (angle b) (pos b) [(-w, -h), (w, -h), (w, h), (-w, h)]
  w = (wheelBase b) / 2.0
  h = w / 3.0


-- Key state handling

setKey :: Bool -> SpecialKey -> Keys -> Keys
setKey b KeyUp ks = ks { up = b } 
setKey b KeyDown ks = ks { down = b } 
setKey b KeyLeft ks = ks { left = b } 
setKey b KeyRight ks = ks { right = b } 
setKey _ _ ks = ks

enable, disable :: SpecialKey -> Keys -> Keys
enable = setKey True
disable = setKey False

