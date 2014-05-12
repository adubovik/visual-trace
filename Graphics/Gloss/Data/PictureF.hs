{-# language
   DeriveFunctor
 , DeriveFoldable
 , DeriveTraversable
 , TupleSections
 #-}

module Graphics.Gloss.Data.PictureF
 ( PictureF(..)
 , Picture
 , PictureA

 , Filling(..)

 -- Smart constructors
 , blank
 , polygon
 , line
 , circle
 , thickCircle
 , arc
 , thickArc
 , text
 , bitmap
 , color
 , translate
 -- , rotate -- isn't yet supported
 , scale
 , pictures

 -- PictureF specific primitives
 , fixHeight
 , fixWidth
 , selectionTrigger
 , vcat
 , rvcat
 , hcat
 , insideRect

 -- Misc
 , getMatrix
 , wrap
 , unWrap
 ) where

import Graphics.Gloss(Path, BitmapData, Color)
import Graphics.Gloss.Data.Matrix
import Graphics.Gloss.Data.Feedback

import Data.Foldable(Foldable)
import Data.Traversable(Traversable)
import Data.Fix
import Data.Monoid

data Filling = Fill | NoFill
  deriving (Eq, Show)

data PictureF a
  -- Primitives
  = Blank
  | Polygon Path
  | Line    Path
  | Circle      Float
  | ThickCircle Float Float
  | Arc         Float Float Float
  | ThickArc    Float Float Float Float
  | Text String
  | Bitmap Int Int BitmapData Bool
  | Color Color a

  -- Transformations
  | Translate Float Float a
  | Rotate Float a
  | Scale Float Float a

  -- Composition
  | Pictures [a]

  -- PictureF specific cases
  | SelectionTrigger (ExWrap Feedback) a

  | FixedSize (Maybe Float) (Maybe Float) a
  | VCat Float [a]
  | HCat Float [a]
  | InsideRect Filling Float (Maybe Color) a
  deriving (Functor, Traversable, Foldable, Eq, Show)

type PictureA a = Fix a PictureF
type Picture    = PictureA ()

getMatrix :: PictureF a -> Matrix
getMatrix pic = case pic of
  Translate x y _ -> identityTranslate (x,y)
  Scale x y _     -> identityScale (x,y)
  Rotate{}        -> error "getMatrix: Rotate isn't yet supported."
  _               -> mempty

wrap :: PictureF Picture -> Picture
wrap = Fix . ((),)

unWrap :: Picture -> PictureF Picture
unWrap = snd . unFix

---------------------
-- Smart constructors.
-- Copied from Graphics.Gloss.Data.Picture.

-- | A blank picture, with nothing in it.
blank :: Picture
blank	= wrap Blank

-- -- | A convex polygon filled with a solid color.
polygon :: Path -> Picture
polygon = wrap . Polygon

-- | A line along an arbitrary path.
line :: Path -> Picture
line 	= wrap . Line

-- | A circle with the given radius.
circle  :: Float -> Picture
circle 	= wrap . Circle

-- | A circle with the given thickness and radius.
--   If the thickness is 0 then this is equivalent to `Circle`.
thickCircle  :: Float -> Float -> Picture
thickCircle = (wrap.) . ThickCircle

-- | A circular arc drawn counter-clockwise between two angles (in degrees)
--   at the given radius.
arc     :: Float -> Float -> Float -> Picture
arc = ((wrap.).) . Arc

-- | A circular arc drawn counter-clockwise between two angles (in degrees),
--   with the given radius  and thickness.
--   If the thickness is 0 then this is equivalent to `Arc`.
thickArc :: Float -> Float -> Float -> Float -> Picture
thickArc = (((wrap.).).) . ThickArc

-- | Some text to draw with a vector font.
text :: String -> Picture
text = wrap . Text

-- | A bitmap image with a width, height and a Vector holding the
--   32-bit RGBA bitmap data.
--
--  The boolean flag controls whether Gloss should cache the data
--  between frames for speed.
--  If you are programatically generating the image for
--  each frame then use `False`.
--  If you have loaded it from a file then use `True`.
bitmap  :: Int -> Int -> BitmapData -> Bool -> Picture
bitmap = (((wrap.).).) . Bitmap

-- | A picture drawn with this color.
color :: Color -> Picture -> Picture
color = (wrap.) . Color

-- | A picture translated by the given x and y coordinates.
translate :: Float -> Float -> Picture -> Picture
translate = ((wrap.).) . Translate

-- -- | A picture rotated clockwise by the given angle (in degrees).
-- rotate  :: Float -> Picture -> Picture
-- rotate = (wrap.) . Rotate

-- | A picture scaled by the given x and y factors.
scale   :: Float -> Float -> Picture -> Picture
scale = ((wrap.).) . Scale

-- | A picture consisting of several others.
pictures :: [Picture] -> Picture
pictures = wrap . Pictures

-- | PictureF specific primitive

-- | Fix absolute height of the picture (in pixels)
fixHeight :: Float -> Picture -> Picture
fixHeight h = wrap . FixedSize Nothing (Just h)

-- | Fix absolute width of the picture (in pixels)
fixWidth :: Float -> Picture -> Picture
fixWidth w = wrap . FixedSize (Just w) Nothing

selectionTrigger :: ExWrap Feedback -> Picture -> Picture
selectionTrigger = (wrap.) . SelectionTrigger

vcat :: Float -> [Picture] -> Picture
vcat _ [] = blank
vcat padding ps = wrap $ VCat padding $ ps

rvcat :: Float -> [Picture] -> Picture
rvcat padding = vcat padding . reverse

hcat :: Float -> [Picture] -> Picture
hcat _ [] = blank
hcat padding ps = wrap $ HCat padding ps

insideRect :: Filling -> Float -> Maybe Color -> Picture -> Picture
insideRect = (((wrap.).).) . InsideRect
