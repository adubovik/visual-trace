{-# language
   FlexibleContexts
 , TypeFamilies
 #-}

module VisualTrace.Protocol.Image
 ( Image(..)
 , interpretCommand

 , stdDrawImage
 , stdDraw
 ) where

import Data.Typeable(Typeable, Typeable1, cast)

import qualified Graphics.Gloss as G
import Graphics.Gloss.Data.ViewPort

import VisualTrace.Data.PictureF.Trans
import VisualTrace.Data.PictureF

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

  onBaseImage :: (Monad m, Typeable b, Typeable1 m) =>
                 (b -> m b) -> (a -> m a)
  onBaseImage f a = case cast f of
    Just f' -> f' a
    Nothing -> return a

stdDrawImage :: Image a => ViewPort -> a -> PictureL
stdDrawImage viewPort = desugarePicture viewPort . drawImageG

stdDraw :: Image a => ViewPort -> a -> G.Picture
stdDraw viewPort = toPicture . flattenPicture . drawImage viewPort

interpretCommand :: Image a => String -> a -> Either String a
interpretCommand command img =
  case reads command of
    [] -> Left $ "Can't read command: " ++ command ++ "."
    (command',_):_ -> Right $ interpret command' img
