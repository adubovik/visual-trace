{-# language
   FlexibleContexts
 , TypeFamilies
 , DeriveDataTypeable
 , RecordWildCards
 #-}

module VisualTrace.Protocol.Image
 ( Image(..)
 , ImageGroup(..)

 , init
 , evolve
 , draw
 , drawSimpl
 , drawWithBorder
 , interpret
 , getFeedbackDataUnderPoint

 , stdGetFeedbackStorage
 , stdDrawBaseSimpl
 , stdDrawAuxSimpl
 , stdDrawBase
 , stdDrawAux

 , onBaseImage
 , onAuxImage
 ) where

import Prelude hiding (init)
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

data ImageGroup a = ImageGroup
  { baseImage :: a
  , auxImage  :: AuxImage a
  }
  deriving Typeable

onBaseImage :: (a -> a) -> ImageGroup a -> ImageGroup a
onBaseImage f group = group { baseImage = f (baseImage group) }

onAuxImage :: (AuxImage a -> AuxImage a) -> ImageGroup a -> ImageGroup a
onAuxImage f group = group { auxImage = f (auxImage group) }

class (Typeable a, Read (Command a)) => Image a where
  type Command a :: *
  data AuxImage a :: *

  initBase :: a
  initAux  :: AuxImage a

  evolveBase :: Float -> a -> a
  evolveAux  :: Float -> AuxImage a -> AuxImage a
  evolveAux  = const id

  interpretBase :: Command a -> a -> a
  interpretAux :: Command a -> AuxImage a -> AuxImage a
  interpretAux = const id

  drawBaseRaw :: a -> PictureG
  drawAuxRaw :: AuxImage a -> PictureG
  drawAuxRaw = const blank

  drawBaseSimpl :: ViewPort -> a -> PictureL
  drawBaseSimpl = stdDrawBaseSimpl

  drawAuxSimpl :: ViewPort -> AuxImage a -> PictureL
  drawAuxSimpl = stdDrawAuxSimpl

  drawBase :: ViewPort -> a -> G.Picture
  drawBase = stdDrawBase

  drawAux :: ViewPort -> AuxImage a -> G.Picture
  drawAux = stdDrawAux

  getFeedbackStorage :: ViewPort -> a -> FeedbackStorage
  getFeedbackStorage = stdGetFeedbackStorage

  showImage :: a -> String
  showImage _ = "showImage"

  onImageGroup :: (Monad m, Typeable b, Typeable1 m) =>
                  (b -> m b) -> (ImageGroup a -> m (ImageGroup a))
  onImageGroup f a = case cast f of
    Just f' -> f' a
    Nothing -> return a

init :: Image a => ImageGroup a
init = ImageGroup { baseImage = initBase
                  , auxImage  = initAux
                  }

evolve :: Image a => Float -> ImageGroup a -> ImageGroup a
evolve secElapsed ImageGroup{..} =
  ImageGroup { baseImage = evolveBase secElapsed baseImage
             , auxImage  = evolveAux  secElapsed auxImage
             }

draw :: Image a => ViewPort -> ImageGroup a -> G.Picture
draw viewPort ImageGroup{..} =
  let base = drawBase viewPort baseImage
      aux  = drawAux  viewPort auxImage
      -- Aux is always above base image
  in  G.pictures [ base, aux ]

drawSimpl :: Image a => ViewPort -> ImageGroup a -> PictureL
drawSimpl viewPort ImageGroup{..} =
  let base = drawBaseSimpl viewPort baseImage
      aux  = drawAuxSimpl  viewPort auxImage
      -- Aux is always above base image
  in  pictures [ base, aux ]

drawWithBorder :: Image a => ViewPort -> ImageGroup a -> Point -> G.Picture
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

getFeedbackDataUnderPoint :: Image a => ViewPort -> ImageGroup a -> Point -> Maybe FeedbackData
getFeedbackDataUnderPoint viewPort image point =
  feedbackDataUnderPoint (getFeedbackStorage viewPort (baseImage image)) point

stdGetFeedbackStorage :: Image a => ViewPort -> a -> FeedbackStorage
stdGetFeedbackStorage viewPort =
  mkFeedbackStorage . buildFeedbackList . drawBaseSimpl viewPort

stdDrawBaseSimpl :: Image a => ViewPort -> a -> PictureL
stdDrawBaseSimpl viewPort = desugarePicture viewPort . drawBaseRaw

stdDrawAuxSimpl :: Image a => ViewPort -> AuxImage a -> PictureL
stdDrawAuxSimpl viewPort = desugarePicture viewPort . drawAuxRaw

stdDrawBase :: Image a => ViewPort -> a -> G.Picture
stdDrawBase viewPort = toPicture . flattenPicture . drawBaseSimpl viewPort

stdDrawAux :: Image a => ViewPort -> AuxImage a -> G.Picture
stdDrawAux viewPort = toPicture . flattenPicture . drawAuxSimpl viewPort

interpret :: Image a => String -> ImageGroup a -> Either String (ImageGroup a)
interpret command ImageGroup{..} =
  case reads command of
    [] -> Left $ "Can't read command: " ++ command ++ "."
    (command',_):_ -> Right $
      ImageGroup { baseImage = interpretBase command' baseImage
                 , auxImage  = interpretAux  command' auxImage
                 }
