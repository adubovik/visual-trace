{-# language
   FlexibleContexts
 , TypeFamilies
 #-}

module VisualTrace.Protocol.Image
 ( Image(..)
 , interpretCommand
 , getFeedbackDataUnderPoint
 , drawWithBorder

 , stdGetFeedbackStorage
 , stdDrawImage
 , stdDraw
 ) where

import Control.Applicative
import Data.Typeable(Typeable, Typeable1, cast)

import qualified Graphics.Gloss as G
import Graphics.Gloss(Point)
import Graphics.Gloss.Data.ViewPort

import VisualTrace.Data.Ext.Utils
import VisualTrace.Data.Picture
import VisualTrace.Data.Picture.Trans
import VisualTrace.Data.Picture.Selection
import VisualTrace.Data.Feedback.FeedbackStorage

class (Typeable a, Read (Command a)) => Image a where
  type Command a :: *

  initImage :: a
  drawImageG :: a -> PictureG
  evolveImage :: Float -> a -> a
  interpret :: Command a -> a -> a

  drawImage :: ViewPort -> a -> PictureL
  drawImage = stdDrawImage

  draw :: ViewPort -> a -> G.Picture
  draw = stdDraw

  getFeedbackStorage :: ViewPort -> a -> FeedbackStorage
  getFeedbackStorage = stdGetFeedbackStorage

  showImage :: a -> String
  showImage _ = "showImage"

  onBaseImage :: (Monad m, Typeable b, Typeable1 m) =>
                 (b -> m b) -> (a -> m a)
  onBaseImage f a = case cast f of
    Just f' -> f' a
    Nothing -> return a

drawWithBorder :: Image a => ViewPort -> a -> Point -> G.Picture
drawWithBorder viewPort image point = picture
  where
    fdData = getFeedbackDataUnderPoint viewPort image point
    imagePicture = draw viewPort image

    picture = case getFdExt <$> fdData of
      Nothing  -> imagePicture
      Just ext -> G.pictures [ imagePicture
                             , toPicture
                             . color G.yellow
                             . drawExt NoFill $ ext
                             ]

getFeedbackDataUnderPoint :: Image a => ViewPort -> a -> Point -> Maybe FeedbackData
getFeedbackDataUnderPoint viewPort image point =
  feedbackDataUnderPoint (getFeedbackStorage viewPort image) point

stdGetFeedbackStorage :: Image a => ViewPort -> a -> FeedbackStorage
stdGetFeedbackStorage viewPort =
  mkFeedbackStorage . buildFeedbackList . drawImage viewPort

stdDrawImage :: Image a => ViewPort -> a -> PictureL
stdDrawImage viewPort = desugarePicture viewPort . drawImageG

stdDraw :: Image a => ViewPort -> a -> G.Picture
stdDraw viewPort = toPicture . flattenPicture . drawImage viewPort

interpretCommand :: Image a => String -> a -> Either String a
interpretCommand command img =
  case reads command of
    [] -> Left $ "Can't read command: " ++ command ++ "."
    (command',_):_ -> Right $ interpret command' img
