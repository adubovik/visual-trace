module VisualTrace.Data.EventInfo.StdLib
 ( stdAnnotationDraw
 , stdAnnotationTransform
 , stdHighlightTransfrom
 ) where

import qualified Graphics.Gloss as G
import qualified VisualTrace.Text as T
import VisualTrace.Data.EventInfo.Utils
import VisualTrace.Data.PictureF

stdAnnotationDraw :: G.Point -> String -> PictureG
stdAnnotationDraw mousePos annotationMsg =
  color G.black $
  translateAnnot $
  T.textWithBackground oneLineHeight (G.greyN 0.8) $
  annotationMsg
  where
    oneLineHeight = Just 20
    translateAnnot =
      let (x,y) = mousePos
      in translate (screen 5) (screen 5) .
         translate (local  x) (local  y)

stdAnnotationTransform ::
  (G.Point -> G.Point -> a -> a) ->
  (a -> a) ->
  Transformer a
stdAnnotationTransform mkAnnotation rmAnnotation =
  onMouseMove mkAnnotation `andWhen`
  onHoverOut  rmAnnotation

stdHighlightTransfrom ::
  (a -> a) ->
  (a -> a) ->
  Transformer a
stdHighlightTransfrom hoverOn hoverOff =
  onHoverIn  hoverOn `andWhen`
  onHoverOut hoverOff

