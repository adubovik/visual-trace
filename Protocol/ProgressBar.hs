{-# language
   RecordWildCards
 , DeriveDataTypeable
 #-}

module Protocol.ProgressBar
 ( Image
 -- Abstract for Server
 , Command(..)
 , mkImage
 , action
 , drawAnn
 , draw
 , evolution
 ) where

import Data.Typeable(Typeable)

import qualified Graphics.Gloss as G
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.PictureF
import Graphics.Gloss.Data.PictureF.Trans
import Graphics.Gloss.Data.Feedback

import Graphics.Gloss.Data.EventInfo.Utils
import Graphics.Gloss.Data.EventInfo.StdLib

data Command = Init Int
             | Done (Maybe String) Int
  deriving (Show, Read, Eq, Ord)

data Image = Image
  { count    :: Int
  , position :: Int
  , annots   :: [Maybe String]
  , cellAnnotation :: Maybe (G.Point, String)
  }
  deriving (Eq, Ord, Show, Read, Typeable)

------------------------------------------------------
-- TODO: make type class which consists of functions below

mkImage :: Image
mkImage = Image { count = 0
                , position = 0
                , annots = []
                , cellAnnotation = Nothing
                }

action :: Command -> Image -> Image
action (Init c)  _i = mkImage { count = c }
action (Done a c) i = i { position = c + position i
                        , annots = annots i ++ replicate c a
                        }

drawAnn :: Image -> Picture
drawAnn Image{..} = pictures (clrRects ++ [annotationPic])
  where
    annotationPic =
      maybe blank (uncurry stdAnnotationDraw) cellAnnotation

    rectSize = 30.0

    clrRects = zipWith color colors rects
    colors = take count $
             map (\i -> if i < position then G.green else G.red) $
             [0..]

    rects = take count $
            zipWith (\i a -> translate (rectSize * fromIntegral i) 0.0 (rect i a))
            [(0::Integer)..] (annots ++ repeat Nothing)

    rect idx ann =
      maybe id (selectionTrigger . cellFeedback idx) ann $
      scale (rectSize*0.95) (rectSize*0.95) $
      polygon [(0,0),(1,0),(1,1),(0,1)]

    cellFeedback idx ann =
      mkFeedback stdFocusCapture feedbackId $
        mkCompFeedback (traceSideEffect feedbackId) transform
      where
        feedbackId = show idx

        transform = stdAnnotationTransform mkAnnotation rmAnnotation

        mkAnnotation _old newPos image =
          image { cellAnnotation = Just (newPos, ann) }
        rmAnnotation image = image { cellAnnotation = Nothing }

evolution :: Float -> Image -> Image
evolution = const id

-- Common logic for all protocols

draw :: ViewPort -> Image -> G.Picture
draw vp = toPicture vp . drawAnn
