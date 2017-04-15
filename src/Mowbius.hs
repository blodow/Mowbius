module Mowbius where

import Algebra.Clipper
import Control.Arrow hiding (left, right)
import Data.IORef
import Data.List
import Data.Ord (comparing)
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import System.Random
import Text.Show.Pretty

import Mowbius.Conversion
import Mowbius.Planner
import Mowbius.Types

run :: IO ()
run = let windowSize = (800, 600) in do
  wInit' <- wInit
  windowSizeRef <- newIORef windowSize
  playIO (InWindow "uMow" windowSize (5, 5)) (greyN 0.2)  -- create gray window
         60 -- fps
         wInit' -- initial world state
         (displayWorldIO windowSizeRef) -- display function
         (handleEventIO windowSizeRef)-- event handler
         advanceIO -- state update function

-- constants

randomFloatsRsIO :: (Float, Float) -> IO [Float]
randomFloatsRsIO range = getStdRandom $ split >>> first (randomRs range)

randPath :: IO Path
randPath = do
  as <- sort . take 10 <$> randomFloatsRsIO (0.0, 360.0)
  rs <- take 10 <$> randomFloatsRsIO (2.5, 5.0)
  return . map polarToCart $ zip as rs
 where
  polarToCart :: (Float, Float) -> (Float, Float)
  polarToCart (a, r) = mapT (r *) (cos (degToRad a), sin (degToRad a))

wInit :: IO World
wInit = randPath >>= \p ->
  clipHoles (f p) >>= \f' -> return $ World f' b none []
 where
  -- Field
  f ps = Field [ps] [h]
  -- p1 = [(-5.0, -5.0), (5.0, -5.0), (5.0, 5.0), (-5.0, 5.0)]
  -- p2 = translatePath (11,0) p1
  h = [] --(1.0, 1.0), (4.0, 4.0), (4.0, 1.0)]
  -- Bot
  b = Bot (5.0, 5.0) 0.0 [] 0.3
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

handleEventIO :: IORef (Int, Int) -> Event -> World -> IO World
handleEventIO _ (EventKey (SpecialKey KeyF1) Down _ _) w = pPrint (decompositions w) >> return w
handleEventIO _ (EventKey (SpecialKey KeyEsc) Down _ _) w = error "ESC pressed."
handleEventIO _ (EventKey (SpecialKey k) Down _ _) w = return w { keys = enable k $ keys w }
handleEventIO _ (EventKey (SpecialKey k) Up _ _) w = return w { keys = disable k $ keys w }
handleEventIO wsr (EventResize ns) w = writeIORef wsr ns >> return w
handleEventIO _ _ w = return w

advanceIO :: Float -> World -> IO World
advanceIO _ w@(World f b k g) = case (s, r) of
 (0, 0) -> return w
 _ -> do
   f' <- mow f b
   return w { field = f', bot = bot', decompositions = map decomp $ fields f' }
 where
  bot' = go s r b
  decomp path = decompose (-angle bot') $ pathToPoly path
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

displayWorldIO :: IORef (Int, Int) -> World -> IO Picture
displayWorldIO wsr w = readIORef wsr >>= \ws -> return $ displayWorld ws w

displayWorld :: (Int, Int) -> World -> Picture
displayWorld ws w@(World f b _ _) = autoScaled ws w $ pictures [ displayBot b
                                                       --, displayField f
                                                       , displayVertexEvents w 
                                                       , displayDecomposition w 
                                                       ]

autoScaled :: (Int, Int) -> World -> Picture -> Picture
autoScaled ws w = Translate tx ty . Scale s s 
 where
  s = 0.9 * minimum [sx, sy]
  sx = ((fromInteger . toInteger . fst) ws) / width worldBounds
  sy = ((fromInteger . toInteger . snd) ws) / height worldBounds
  worldBounds = getBounds w
  center (p, q) = (mulSV 0.5 (q - p)) + p  -- compute center between two points
  c = center $ mapT (mulSV s) worldBounds -- scale worldbounds and get center
  tx = - fst c
  ty = - snd c

  width :: (Point, Point) -> Float
  width (p, q) = fst (q - p)
  height :: (Point, Point) -> Float
  height (p, q) = snd (q - p)

displayVertexEvents :: World -> Picture
displayVertexEvents w@(World f b _ _) = Pictures . map displayVertexEvents' $ fields f
 where
  displayVertexEvents' path = Pictures .  map draw $ zip (evs $ pathToPoly path) path
  evs p = eventsForDirection (-angle b) p
  draw :: (VertexEvent, Point) -> Picture
  draw (InHull, (x, y)) = translate x y . Color green $ sText "Iu"
  draw (InHole, (x, y)) = translate x y . Color white $ sText "Io"
  draw (OutHull, (x, y)) = translate x y . Color red $ sText "Ou"
  draw (OutHole, (x, y)) = translate x y . Color cyan $ sText "Oo"
  draw (Middle, (x, y)) = translate x y . Color (greyN 0.9) $ sText "m"
  draw (_, _) = blank

sText :: String -> Picture
sText = Scale 0.005 0.005 . Text

displayDecomposition :: World -> Picture
displayDecomposition w@(World f b _ ds) = Pictures $ map draw ds
 where
  draw :: Graph -> Picture
  draw g = Pictures $ polys (grNodes g) ++ tags (grNodes g) ++ edges

  tags :: [TaggedPolygon] -> [Picture]
  tags = map (\tp -> uncurry translate (centroid $ tpyPoints tp) . Color yellow . sText . show $ tpyTag tp)
  centroid :: [Point] -> Point
  centroid [] = (0, 0)
  centroid ts = dividePt (sum ts) (genericLength ts)
  dividePt (x,y) l = (x/l, y/l)

  edges = []

  polys :: [TaggedPolygon] -> [Picture]
  polys = map ((Color cyan) . lineLoop . tpyPoints)

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
  allPoints = pad (pos (bot w)) ++ (concat . fields $ field w)
  (xs, ys) = unzip allPoints
  pad (x, y) = [(x-1,y-1), (x+1,y+1)]

-- Mowing

mow :: Field -> Bot -> IO Field
mow f b = toPaths <$> clip ps bot' 
                  >>= \f' -> return f { fields = simplifyPaths f' }
 where
  clip = execute ctDifference 
  ps = pathsToPolys $ fields f
  bot' = Polygons $ [pathToPoly botFootPrint]
  botFootPrint = transformPath (angle b) (pos b) [(-w, -h), (w, -h), (w, h), (-w, h)]
  w = (wheelBase b) / 2.0
  h = w / 3.0

simplifyPaths :: [Path] -> [Path]
simplifyPaths = map $ rdp 0.01 -- 1cm threshold
 where
  -- Ramer-Douglas-Peucker split-and-merge algorithm
  rdp :: Float -> Path -> Path
  rdp e p | length p <= 2 = p
          | peak <= e     = [head p, last p]
          | otherwise     = front ++ tail back
        where
          front = rdp e $ take (loc+1) p
          back = rdp e $ drop loc p
          (loc, peak) = peakDist (head p) (last p) p

  peakDist :: Point -> Point -> Path -> (Int, Float)
  peakDist _ _ [] = (-1, 0)
  peakDist (ax, ay) (bx, by) ps =
    maximumBy (comparing snd) . zip [1..] . flip map (tail $ init ps) $
     \(cx, cy) -> let denom = sqrt((bx - ax)^2 + (by - ay)^2)
                      term = bx*ay - by*ax
                   in abs ((by-ay)*cx - (bx-ax)*cy + term) / denom

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

