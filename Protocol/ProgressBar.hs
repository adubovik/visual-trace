{-# language
   RecordWildCards
 #-}

module Protocol.ProgressBar where

import Graphics.Gloss

-- TODO: Split into a few sub-modules, 1. render 2. logic 3. data decl ...

data Command = Init Int
             | Done Int
             | Reset Int
  deriving (Show, Read, Eq, Ord)

data Image = Image
  { count :: Int
  , position :: Int
  }

-- TODO: make type class of these 3 functions probably
mkImage :: Image
mkImage = Image 0 0

action :: Command -> Image -> Image
action (Init  c) i = i { count    = c, position = 0 }
action (Done  c) i = i { position = c + position i }
action (Reset c) i = i { position = c }

draw :: Image -> Picture
draw Image{..} = pictures clrRects
  where
    clrRects = zipWith color colors rects
    colors = take count $
             map (\i -> if i < position then green else red) $
             [0..]
    rects = take count $
            zipWith (\r i -> translate (fromIntegral i) 0.0 r)
            (repeat rect) [(0::Integer)..]
    rect = scale 0.95 0.95 $
           polygon [(0,0),(1,0),(1,1),(0,1)]
