{-# language
   RecordWildCards
 , DeriveDataTypeable
 , TypeFamilies
 #-}

module VisualTrace.Protocol.Circles
 ( Image
 -- Abstract for Server
 , Command(..)
 , Circle(..)
 )
 where

import Data.Typeable
import qualified Data.Map as Map

import Graphics.Gloss.Data.Color(white)
import Graphics.Gloss.Interface.Pure.Game(MouseButton(..))
import Graphics.Gloss.Data.Point

import VisualTrace.Data.EventInfo.Utils
import VisualTrace.Data.EventInfo.StdLib
import VisualTrace.Data.Picture hiding (circle)
import qualified VisualTrace.Data.Picture as P

import VisualTrace.Data.ColorRead(Color,toColor)
import VisualTrace.Data.Feedback
import qualified VisualTrace.Protocol.Image as I
import VisualTrace.Protocol.Image(ImageGroup,AuxImage,onBaseImage)

type CircleId = String
data Command = InitCircle CircleId Circle
             | ChangeRaduis CircleId Float
  deriving (Show, Read)

data Circle = Circle
  { cPos :: Point
  , cRad :: Float
  , _cClr :: Color
  }
  deriving (Show, Read)

data Image = Image
  { circles :: Map.Map CircleId Circle
  , circleAnnotation :: Maybe (Point, String)
  , circleHighlighted :: Maybe CircleId
  }
  deriving (Show, Read, Typeable)

instance I.Image Image where
  type Command Image = Command
  data AuxImage Image = NilAuxImage
  initBase = initBase
  initAux  = NilAuxImage

  drawBaseRaw = drawBaseRaw
  evolveBase = evolveBase
  interpretBase = interpretBase

evolveBase :: Float -> Image -> Image
evolveBase _secElapsed = id

interpretBase :: Command -> Image -> Image
interpretBase (InitCircle ident circle) image =
  image { circles = Map.insert ident circle (circles image) }
interpretBase (ChangeRaduis ident rad) image =
  image { circles = Map.adjust (changeRad rad) ident (circles image) }
  where
    changeRad r circle = circle { cRad = r }

initBase :: Image
initBase = Image { circles = Map.empty
                 , circleAnnotation = Nothing
                 , circleHighlighted = Nothing
                 }

adjustCirclePos :: CircleId -> Point -> Image -> Image
adjustCirclePos ident newPos image =
  image { circles = Map.adjust (adjust newPos) ident (circles image) }
  where
    adjust pos circle = circle { cPos = pos }

drawBaseRaw :: Image -> PictureG
drawBaseRaw Image{..} =
  pictures $ annotationPic : circlePics
  where
    annotationPic = maybe blank annotationDraw circleAnnotation
    annotationDraw (mousePos, msg) = stdAnnotationDraw mousePos msg

    circlePics = map (uncurry drawCircle) $ Map.toList circles

    drawCircle ident c@(Circle (x,y) radius clr) =
      toPictureG $
        selectionTrigger (circleFeedback ident c) $
          color clr' $
            P.circle (x,y) radius
      where
        clr' | Just ident == circleHighlighted = white
             | otherwise = toColor clr

    circleFeedback :: CircleId -> Circle -> Feedback (ImageGroup Image)
    circleFeedback ident (Circle _ rad _) =
      mkFeedback focusCapture feedbackId $
        mkCompFeedback (traceSideEffect feedbackId) transform
      where
        feedbackId = ident
        focusCapture = keepFocusedIf (mouseButtonDrag LeftButton)

        transform =
          stdHighlightTransfrom  hoverOn      hoverOff     `andWhen`
          stdAnnotationTransform mkAnnotation rmAnnotation `andWhen`
          onMouseDrag LeftButton moveCircle

        hoverOn  = onBaseImage $ \image -> image { circleHighlighted = Just ident }
        hoverOff = onBaseImage $ \image -> image { circleHighlighted = Nothing    }

        rmAnnotation = onBaseImage $ \image ->
          image { circleAnnotation = Nothing }
        mkAnnotation _oldPos newPos = onBaseImage $ \image ->
          image { circleAnnotation = Just (newPos, msg) }
          where
            msg = ident ++ ", " ++ show rad

        moveCircle _oldPos newPos = onBaseImage $
          adjustCirclePos ident newPos
