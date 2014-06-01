module VisualTrace.Text (
   text
 , textWithBackground
 , textsWithBackground

 , textHeight
 , textWidth
 ) where

import System.IO.Unsafe

import Control.Arrow

import Graphics.Gloss(Color)
import VisualTrace.Data.Picture hiding (text)
import qualified VisualTrace.Data.Picture as PF
import qualified Graphics.UI.GLUT as GLUT

-- Greatly depends on font (namely GLUT.Roman) that is
-- used to render Text primitive in Gloss.
fontHeight :: Float
fontHeight = fromIntegral .
             (round :: GLUT.GLfloat -> Integer) .
             unsafePerformIO $
             GLUT.fontHeight GLUT.Roman

textHeight :: String -> Float
textHeight = const fontHeight

textWidth :: String -> Float
textWidth str = fromIntegral . unsafePerformIO $
                GLUT.stringWidth GLUT.Roman str

textsWithBackground :: Maybe Float -> [(Color, String)] -> PictureG
textsWithBackground lineHeight rows =
  rvcat (local 0) $
    map (uncurry $ textWithBackground lineHeight) rows

textWithBackground :: Maybe Float -> Color -> String -> PictureG
textWithBackground lineHeight clr str = let (_len, pic) = textMultiLine textOneLine str
                                        in  fxHeight lineHeight pic
  where
    fxHeight Nothing  = id
    fxHeight (Just h) = scale' (screen (h / height))

    width  = textWidth str
    height = textHeight str

    textOneLine msg = toPictureG $
      pictures [ color clr $
                   polygon [ (0,0)
                           , (width,0)
                           , (width,height)
                           , (0,height)
                           ]
               , PF.text (0,0) msg
               ]

text :: String -> PictureG
text = snd . textMultiLine (PF.text (local 0, local 0))

textMultiLine :: (String -> PictureG) -> String -> (Int, PictureG)
textMultiLine textOneLine =
  (length &&& (pictures . zipWith textWithOffset [0..])) . lines
  where
    textWithOffset idx str =
      translate (local 0.0) (local (-idx*fontHeight)) $
        textOneLine str
