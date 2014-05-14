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
import qualified Data.Map as Map
import Data.Maybe
import Text.Printf

import qualified Graphics.Gloss as G
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.PictureF
import Graphics.Gloss.Data.PictureF.Trans
import Graphics.Gloss.Data.Feedback

import Graphics.Gloss.Data.EventInfo.Utils
import Graphics.Gloss.Data.EventInfo.StdLib

data Command = Init ProgressBarId (Maybe Int)
             | Done ProgressBarId (Maybe String) Int
  deriving (Show, Read, Eq, Ord)

data Image = Image
  { progressBars   :: Map.Map ProgressBarId ProgressBar
  , cellAnnotation :: Maybe (Point, String)
  }
  deriving (Eq, Ord, Show, Read, Typeable)

type ProgressBarId = String

data ProgressBar = ProgressBar
  { count    :: Maybe Int
  , position :: Int
  , annots   :: [Maybe String]
  }
  deriving (Eq, Ord, Show, Read)

initProgressBar :: ProgressBar
initProgressBar = ProgressBar
  { count = Nothing
  , position = 0
  , annots = []
  }

mkImage :: Image
mkImage = Image { progressBars = Map.empty
                , cellAnnotation = Nothing
                }

action :: Command -> Image -> Image
action (Init pid len) i =
  let pb = initProgressBar { count = len }
  in  i { progressBars = Map.insert pid pb $ progressBars i }

action (Done pid ann done) i =
  i { progressBars = Map.adjust (progress ann done) pid $ progressBars i }
  where
    progress a d pb =
      pb { position = d + position pb
         , annots = annots pb ++ replicate d a
         }

drawAnn :: Image -> Picture
drawAnn Image{..} = pictures $ [ progressBarPics
                               , annotationPic
                               ]
  where
    progressBarPics =
      rvcat padding $
      map (uncurry drawProgressBar) $
      Map.toList progressBars

    annotationPic =
      maybe blank (uncurry stdAnnotationDraw) cellAnnotation

    drawProgressBar _pid ProgressBar{..}
      | annots == []
      , count == Nothing
      = color G.red drawContour

    drawProgressBar pid ProgressBar{..} = hcat padding clrRects
      where
        nRects = fromMaybe (length annots) count
        clrRects = take nRects $
                   zipWith color colors rects

        colors = map (\idx -> if idx < position
                              then G.green
                              else G.red
                     ) [0..]

        rects = zipWith (drawCell pid)
                  [(0::Integer)..]
                  (annots ++ repeat Nothing)

    drawCell pid idx ann =
      maybe id (selectionTrigger . cellFeedback pid idx) ann $
      scale cellSize cellSize $
      polygon [(0,0),(1,0),(1,1),(0,1)]

    drawContour =
      scale cellSize cellSize $
      line [(0,0),(1,0),(1,1),(0,1),(0,0)]

    padding = cellSize/15.0
    cellSize = 30.0

    cellFeedback pid idx ann =
      mkFeedback stdFocusCapture feedbackId $
        mkCompFeedback (traceSideEffect feedbackId) transform
      where
        feedbackId = printf "%s_%d" pid idx

        transform = stdAnnotationTransform mkAnnotation rmAnnotation

        mkAnnotation _old newPos image =
          image { cellAnnotation = Just (newPos, pid ++ ": " ++ ann) }
        rmAnnotation image = image { cellAnnotation = Nothing }

evolution :: Float -> Image -> Image
evolution = const id

-- Common logic for all protocols

draw :: ViewPort -> Image -> G.Picture
draw vp = toPicture vp . drawAnn
