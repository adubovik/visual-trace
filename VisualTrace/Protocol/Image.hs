{-# language
   TypeFamilies
 , FlexibleContexts
 #-}

module VisualTrace.Protocol.Image
 ( Image(..)
 , interpretCommand
 , drawImage
 , draw
 ) where

import Data.Typeable(Typeable)
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

interpretCommand :: Image a => String -> a -> Either String a
interpretCommand command img =
  case reads command of
    [] -> Left $ "Can't read command: " ++ command ++ "."
    (command',_):_ -> Right $ interpret command' img

drawImage :: Image a => ViewPort -> a -> PictureL
drawImage viewPort = desugarePicture viewPort . drawImageG

draw :: Image a => ViewPort -> a -> G.Picture
draw viewPort = toPicture . drawImage viewPort

