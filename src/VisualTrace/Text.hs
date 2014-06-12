{-# language
   ViewPatterns
 #-}

module VisualTrace.Text (
   text
 , textWithBackground
 , textsWithBackground

 , textHeight
 , textWidth
 , textScreen
 ) where

import System.IO.Unsafe

import Control.Arrow

import qualified Graphics.Gloss as G
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

toFloat :: Int -> Float
toFloat = fromIntegral

textScreen :: (Int,Int) -> (Int,Int) -> String -> G.Picture
textScreen (toFloat -> screenX, toFloat -> screenY)
           (toFloat ->    winX, toFloat ->    winY)
           msg = msgPic
  where
    height = 15.0
    nLines = toFloat $ length $ lines msg

    scaleFactor = height/fontHeight

    offsetX = -winX/2.0 + screenX
    offsetY =  winY/2.0 - screenY - nLines*height

    linesPics = G.pictures $
                map drawLine $
                zip [0..] $
                reverse $ lines msg

    drawLine (idx,msgLine) =
      G.translate 0.0 (idx * height) $
      G.scale scaleFactor scaleFactor $
      G.text msgLine

    msgPic = G.color G.white $
             G.translate offsetX offsetY $
             linesPics

