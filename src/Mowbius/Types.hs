module Mowbius.Types
  ( Field(..)
  , Bot(..)
  , Keys(..)
  , World(..)
  ) where

import Graphics.Gloss

data Field = Field { fields :: [Path]
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

data World = World { field :: Field
                   , bot :: Bot
                   , keys :: Keys}

