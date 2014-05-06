module Graphics.Gloss.Text (
   text
 , textWithBackground
 , textsWithBackground

 , textHeight
 , textWidth
 ) where

import System.IO.Unsafe

import Control.Arrow

import Graphics.Gloss(Color)
import Graphics.Gloss.Data.PictureF hiding (text)
import qualified Graphics.Gloss.Data.PictureF as PF
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

textsWithBackground :: Maybe Float -> [(Color, String)] -> Picture
textsWithBackground lineHeight rows =
  rvcat 0 $ map (uncurry $ textWithBackground lineHeight) rows

textWithBackground :: Maybe Float -> Color -> String -> Picture
textWithBackground lineHeight clr str = let (len, pic) = textMultiLine textOneLine str
                                        in  fxHeight len lineHeight pic
  where
    fxHeight _Lines Nothing  = id
    fxHeight nLines (Just h) = fixHeight (fromIntegral nLines * h)

    textOneLine msg =
      pictures [ color clr $
                   polygon [ (0,0)
                           , (width,0)
                           , (width,height)
                           , (0,height)
                           ]
               , PF.text msg
               ]
      where
        width  = textWidth str
        height = textHeight str

text :: String -> Picture
text = snd . textMultiLine PF.text

textMultiLine :: (String -> Picture) -> String -> (Int, Picture)
textMultiLine textOneLine =
  (length &&& (pictures . zipWith textWithOffset [0..])) . lines
  where
    textWithOffset idx str =
      translate 0.0 (-idx*fontHeight) $ textOneLine str
