module Lib (run) where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Pure.Game

data Area = Area { areas :: [Path]
                 , holes :: [Path]
                 }

data Bot = Bot { pos :: Point
               , angle :: Float
               , path :: Path
               , wheelBase :: Float 
               }

data Keys = Keys { up :: Bool
                 , down :: Bool
                 , left :: Bool
                 , right :: Bool
                 }

data World = World { area :: Area, bot :: Bot , keys :: Keys}
 
run = play (InWindow "uMow" (800, 600) (5, 5)) (greyN 0.2)  -- create gray window
           60 -- fps
           wInit -- initial world state
           (Scale 20 20 . displayWorld) -- display function
           handleEvent -- event handler
           advance -- state update function

-- constants

wInit :: World
wInit = World a b none
 where
  -- Area
  a = Area [p1, p2] [h]
  p1 = [(-5.0, -5.0), (5.0, -5.0), (5.0, 5.0), (-5.0, 5.0)]
  p2 = translatePath (11,0) p1
  h = [(1.0, 1.0), (4.0, 4.0), (4.0, 1.0)]
  -- Bot
  b = Bot (0.0, 0.0) 0.0 [] 0.3
  -- Keys
  none = Keys False False False False

speed :: Float
speed = 0.05

rotSpeed :: Float
rotSpeed = 2
              
-- functions

handleEvent :: Event -> World -> World
handleEvent (EventKey (SpecialKey k) Down _ _) w = w { keys = enable k $ keys w }
handleEvent (EventKey (SpecialKey k) Up _ _) w = w { keys = disable k $ keys w }
handleEvent _ w = w

advance :: Float -> World -> World
advance _ w@(World _ b k) = w { bot = go (step k) (rot k) b }
 where
  step (Keys True False _ _) = speed -- forwards
  step (Keys False True _ _) = -speed -- backwards
  step _ = 0
  rot (Keys _ _ True False) = -rotSpeed -- left
  rot (Keys _ _ False True) = rotSpeed -- right
  rot _ = 0

-- move robot by rotating r and taking a step with magniture t
go :: Float -> Float -> Bot -> Bot
go t r b = b { pos = p, angle = r', path = take 200 $ p : path b}
 where
  r' = r + angle b
  p = translatePoint (pos b) step
  step = rotateV (-rad) (0, t)
  rad = r' / 180.0 * pi

-- Display functions

displayWorld :: World -> Picture
displayWorld (World a b _) = pictures [ displayArea a, displayBot b ]

displayArea :: Area -> Picture
displayArea Area {areas=as, holes=hs} = pictures [as', hs']
 where
  as' = Color red . pictures $ map lineLoop as
  hs' = Color green . pictures $ map lineLoop hs

displayBot :: Bot -> Picture
displayBot b = Scale 1 1 $ pictures [path', t $ r botp]
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
                  ]

-- Helper functions

translatePath :: Point -> Path -> Path
translatePath t = map $ translatePoint t

translatePoint :: Point -> Point -> Point
translatePoint (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Key state handling

setKey :: Bool -> SpecialKey -> Keys -> Keys
setKey b KeyUp ks = ks { up = b } 
setKey b KeyDown ks = ks { down = b } 
setKey b KeyLeft ks = ks { left = b } 
setKey b KeyRight ks = ks { right = b } 
setKey _ _ ks = ks

enable :: SpecialKey -> Keys -> Keys
enable = setKey True

disable :: SpecialKey -> Keys -> Keys
disable = setKey False
