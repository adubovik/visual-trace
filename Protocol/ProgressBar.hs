{-# language
   RecordWildCards
 #-}

module Protocol.ProgressBar where

import qualified Graphics.Gloss as G
import Graphics.Gloss.Data.PictureF hiding (getAnn)
import Graphics.Gloss.Data.PictureF.Selection

-- TODO: Split into a few sub-modules, 1. render 2. logic 3. data decl ...

data Command = Init Int
             | Done (Maybe String) Int
  deriving (Show, Read, Eq, Ord)

data Image = Image
  { count    :: Int
  , position :: Int
  , annots   :: [Maybe String]
  }
  deriving (Eq, Ord, Show, Read)

------------------------------------------------------
-- TODO: make type class which consists of functions below

mkImage :: Image
mkImage = Image 0 0 []

action :: Command -> Image -> Image
action (Init c) i = i { count = c
                      , position = 0
                      , annots = [] }
action (Done a c) i = i { position = c + position i
                        , annots = annots i ++ replicate c a
                        }

getAnn :: (Float, Float) -> Image -> Maybe String
getAnn mousePos = select mousePos . drawAnn

draw :: Image -> G.Picture
draw = toPicture . drawAnn

drawAnn :: Image -> Picture (Maybe String)
drawAnn Image{..} = pictures clrRects
  where
    clrRects = zipWith color colors rects
    colors = take count $
             map (\i -> if i < position then G.green else G.red) $
             [0..]
    rects = take count $
            zipWith (\i a -> translate (fromIntegral i) 0.0 (rect a))
            [(0::Integer)..] (annots ++ repeat Nothing)
    rect a = annotate a $
             scale 0.95 0.95 $
             polygon [(0,0),(1,0),(1,1),(0,1)]
