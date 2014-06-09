{-# language
   RecordWildCards
 , DeriveDataTypeable
 , TypeFamilies
 , PatternGuards
 #-}

module VisualTrace.Protocol.ProgressBar
 ( Image()
 -- Abstract for Server
 , Command(..)
 ) where

import Data.Typeable(Typeable)
import qualified Data.Map as Map
import Data.Maybe
import Text.Printf

import qualified Graphics.Gloss as G
import Graphics.Gloss.Data.Point

import VisualTrace.Data.Picture
import VisualTrace.Data.Feedback
import VisualTrace.Data.EventInfo.Utils
import VisualTrace.Data.EventInfo.StdLib
import qualified VisualTrace.Protocol.Image as I
import VisualTrace.Protocol.Image(ImageGroup,onBaseImage)

type ProgressBarId = String

data Command = Init ProgressBarId (Maybe Int)
             | Done ProgressBarId (Maybe String) Int
  deriving (Show, Read, Eq, Ord)

data Image = Image
  { progressBars   :: Map.Map ProgressBarId ProgressBar
  , cellAnnotation :: Maybe (Point, String)
  }
  deriving (Eq, Ord, Show, Read, Typeable)

instance I.Image Image where
  type Command Image = Command
  data AuxImage Image = NilAuxImage

  initBase = initBase
  initAux  = NilAuxImage

  drawBaseRaw   = drawBaseRaw
  evolveBase    = evolveBase
  interpretBase = interpretBase

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

initBase :: Image
initBase = Image { progressBars = Map.empty
                  , cellAnnotation = Nothing
                  }

interpretBase :: Command -> Image -> Image
interpretBase (Init pid len) i =
  let pb = initProgressBar { count = len }
  in  i { progressBars = Map.insert pid pb $ progressBars i }

interpretBase (Done pid ann done) i =
  i { progressBars = Map.adjust (progress ann done) pid $ progressBars i }
  where
    progress a d pb =
      pb { position = d + position pb
         , annots = annots pb ++ replicate d a
         }

drawBaseRaw :: Image -> PictureG
drawBaseRaw Image{..} = pictures $ [ progressBarPics
                                   , annotationPic
                                   ]
  where
    padding  = screen $ cellSize/15.0
    cellSize = 30.0

    progressBarPics =
      rvcat padding $
      map (uncurry drawProgressBar) $
      Map.toList progressBars

    annotationPic =
      maybe blank (uncurry stdAnnotationDraw) cellAnnotation

    drawProgressBar _pid ProgressBar{..}
      | annots == []
      , count == Nothing
      = color (G.light G.red) drawContour

    drawProgressBar pid ProgressBar{..} = hcat padding clrRects
      where
        nRects = fromMaybe (length annots) count
        clrRects = take nRects $
                   zipWith color colors rects

        colors = map (\idx -> if idx < position
                              then (G.light G.green)
                              else (G.light G.red)
                     ) [0..]

        rects = zipWith (drawCell pid)
                  [(0::Integer)..]
                  (annots ++ repeat Nothing)

    drawCell pid idx ann =
      maybe id (selectionTrigger . cellFeedback pid idx) ann $
      toPictureG $
      scale cellSize cellSize $
      polygon [(0,0),(1,0),(1,1),(0,1)]

    drawContour =
      toPictureG $
      scale cellSize cellSize $
      line [(0,0),(1,0),(1,1),(0,1),(0,0)]

    cellFeedback :: String -> Integer -> String ->
                    Feedback (ImageGroup Image)
    cellFeedback pid idx ann =
      mkFeedback stdFocusCapture feedbackId $
        mkCompFeedback (traceSideEffect feedbackId) transform
      where
        feedbackId = printf "%s_%d" pid idx

        transform = stdAnnotationTransform mkAnnotation rmAnnotation

        mkAnnotation _old newPos = onBaseImage $ \image ->
          image { cellAnnotation = Just (newPos, pid ++ ": " ++ ann) }
        rmAnnotation = onBaseImage $ \image ->
          image { cellAnnotation = Nothing }

evolveBase :: Float -> Image -> Image
evolveBase = const id
