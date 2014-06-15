module VisualTrace.Data.Picture.OpenGL
 ( toOpenGL
 , generateOpenGLApp
 ) where

import Text.Printf
import System.FilePath

import Graphics.Gloss(Point)
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture(Picture(..))

import VisualTrace.Data.Picture.OpenGLTemplate

glMode :: String -> String -> String
glMode mode body = unlines
  [ printf "glBegin(%s);" mode
  , body
  , printf "glEnd();"
  ]

glColor :: Color -> String -> String
glColor clr body = unlines
  [ printf "glColor3f(%f,%f,%f);" r g b
  , body
  ]
  where
    (r,g,b,_a) = rgbaOfColor clr

glTranslate :: Float -> Float -> String -> String
glTranslate x y body = unlines
  [ printf "glPushMatrix();"
  , printf "glTranslatef(%f,%f,0.0f);" x y
  , body
  , printf "glPopMatrix();"
  ]

glScale :: Float -> Float -> String -> String
glScale x y body = unlines
  [ printf "glPushMatrix();"
  , printf "glScalef(%f,%f,1.0f);" x y
  , body
  , printf "glPopMatrix();"
  ]

pathToOpenGL :: [Point] -> String
pathToOpenGL = unlines . map point

point :: Point -> String
point (x,y) = printf "glVertex2f(%f,%f);" x y

toOpenGL :: Picture -> String
toOpenGL picture = case picture of
  Blank             -> "/* Blank */"
  Polygon path      -> glMode "GL_POLYGON" $ pathToOpenGL path
  Line path         -> glMode "GL_LINE_STRIP" $ pathToOpenGL path
  Circle radius     -> printf "/* drawCircle(%f); */" radius -- FIXME!
  ThickCircle{}     -> error "ThickCircle isn't yet supported."
  Arc{}             -> error "Arc isn't yet supported."
  ThickArc{}        -> error "ThickArc isn't yet supported."
  Text msg          -> printf "/* Text %s */" msg -- FIXME!
  Bitmap{}          -> error "Bitmap isn't yet supported."
  Color clr pic     -> glColor clr $ toOpenGL pic
  Translate x y pic -> glTranslate x y $ toOpenGL pic
  Scale x y pic     -> glScale x y $ toOpenGL pic
  Rotate{}          -> error "Rotate isn't yet supported."
  Pictures pics     -> unlines $ map toOpenGL pics

generateOpenGLApp :: Picture -> FilePath -> IO ()
generateOpenGLApp picture outFile = do
  let glCode = toOpenGL picture
      outFileName = takeFileName outFile
  writeFile outFile (printf openGLTemplate outFileName glCode)
