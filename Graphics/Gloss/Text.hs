module Graphics.Gloss.Text (
   text
 , textWithBackground

 , textHeight
 , textWidth
 ) where

import System.IO.Unsafe

import Graphics.Gloss hiding (text)
import qualified Graphics.Gloss as G
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

textWithBackground :: Color -> String -> Picture
textWithBackground clr = textMultiLine textOneLine
  where
    textOneLine str =
      pictures [ color clr $
                   polygon [ (0,0)
                           , (width,0)
                           , (width,height)
                           , (0,height)
                           ]
               , G.text str
               ]
      where
        width  = textWidth str
        height = textHeight str

text :: String -> Picture
text = textMultiLine G.text

textMultiLine :: (String -> Picture) -> String -> Picture
textMultiLine textOneLine =
  pictures . zipWith textWithOffset [0..] . lines
  where
    textWithOffset idx str =
      translate 0.0 (-idx*fontHeight) $ textOneLine str
