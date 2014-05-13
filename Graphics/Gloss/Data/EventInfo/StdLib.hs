module Graphics.Gloss.Data.EventInfo.StdLib
 ( stdAnnotationDraw
 , stdAnnotationTransform
 , stdHighlightTransfrom
 ) where

import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Text as T
import Graphics.Gloss.Data.EventInfo.Utils
import Graphics.Gloss.Data.PictureF

stdAnnotationDraw :: G.Point -> String -> Picture
stdAnnotationDraw mousePos annotationMsg =
  color G.black $
  uncurry translate annotPos $
  T.textWithBackground oneLineHeight (G.greyN 0.8) $
  annotationMsg
  where
    oneLineHeight = Just 20
    -- TODO: + (20,20) in terms of real screen coordinates
    annotPos = mousePos + (20,20)

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

