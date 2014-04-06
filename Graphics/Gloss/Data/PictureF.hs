{-# language
   DeriveFunctor
 , DeriveFoldable
 , DeriveTraversable
 , TypeOperators
 , NoMonomorphismRestriction
 #-}

module Graphics.Gloss.Data.PictureF where

import qualified Graphics.Gloss as G
import Graphics.Gloss(Path, BitmapData, Color)

import Data.Foldable(Foldable)
import qualified Data.Foldable as Foldable
import Data.Traversable(Traversable)
import qualified Data.Traversable as Traversable
import Data.Fix
import Data.Monoid

import Control.Monad.Reader

data PictureF a
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
  | Translate Float Float a
  | Rotate Float a
  | Scale Float Float a
  | Pictures [a]
  deriving (Functor, Traversable, Foldable, Show, Eq)

type Picture ann = Fix (K ann :*: PictureF)

annotate :: ann -> Picture ann -> Picture ann
annotate a = Fix . ann a .  deAnn . unFix

getAnn :: (K ann :*: f) a -> ann
getAnn (K x :*: _y) = x

deAnn :: (K ann :*: f) a -> f a
deAnn (K _x :*: y) = y

ann :: ann -> f a -> (K ann :*: f) a
ann a y = K a :*: y

annZero :: Monoid ann => f a -> (K ann :*: f) a
annZero = ann mempty

wrap :: Monoid ann => PictureF (Picture ann) -> Picture ann
wrap = Fix . annZero

toPicture :: Picture ann -> G.Picture
toPicture = cata (alg . deAnn)
  where
    alg Blank              = G.Blank
    alg (Polygon p)        = G.Polygon p
    alg (Line p)           = G.Line p
    alg (Circle a)         = G.Circle a
    alg (ThickCircle a b)  = G.ThickCircle a b
    alg (Arc a b c)        = G.Arc a b c
    alg (ThickArc a b c d) = G.ThickArc a b c d
    alg (Text s)           = G.Text s
    alg (Bitmap a b c d)   = G.Bitmap a b c d
    alg (Color c a)        = G.Color c a
    alg (Translate a b c)  = G.Translate a b c
    alg (Rotate a b)       = G.Rotate a b
    alg (Scale a b c)      = G.Scale a b c
    alg (Pictures p)       = G.Pictures p

fromPicture :: Monoid ann => G.Picture -> Picture ann
fromPicture = ana (annZero . coalg)
  where
    coalg G.Blank              = Blank
    coalg (G.Polygon p)        = Polygon p
    coalg (G.Line p)           = Line p
    coalg (G.Circle a)         = Circle a
    coalg (G.ThickCircle a b)  = ThickCircle a b
    coalg (G.Arc a b c)        = Arc a b c
    coalg (G.ThickArc a b c d) = ThickArc a b c d
    coalg (G.Text s)           = Text s
    coalg (G.Bitmap a b c d)   = Bitmap a b c d
    coalg (G.Color c a)        = Color c a
    coalg (G.Translate a b c)  = Translate a b c
    coalg (G.Rotate a b)       = Rotate a b
    coalg (G.Scale a b c)      = Scale a b c
    coalg (G.Pictures p)       = Pictures p

---------------------
-- Smart constructors.
-- Copied from Graphics.Gloss.Data.Picture.

-- | A blank picture, with nothing in it.
blank :: Monoid a => Picture a
blank	= wrap Blank

-- -- | A convex polygon filled with a solid color.
polygon :: Monoid a => Path -> Picture a
polygon = wrap . Polygon

-- | A line along an arbitrary path.
line :: Monoid a => Path -> Picture a
line 	= wrap . Line

-- | A circle with the given radius.
circle  :: Monoid a => Float  -> Picture a
circle 	= wrap . Circle

-- | A circle with the given thickness and radius.
--   If the thickness is 0 then this is equivalent to `Circle`.
thickCircle  :: Monoid a => Float -> Float -> Picture a
thickCircle = (wrap.) . ThickCircle

-- | A circular arc drawn counter-clockwise between two angles (in degrees)
--   at the given radius.
arc     :: Monoid a => Float -> Float -> Float -> Picture a
arc = ((wrap.).) . Arc

-- | A circular arc drawn counter-clockwise between two angles (in degrees),
--   with the given radius  and thickness.
--   If the thickness is 0 then this is equivalent to `Arc`.
thickArc :: Monoid a => Float -> Float -> Float -> Float -> Picture a
thickArc = (((wrap.).).) . ThickArc

-- | Some text to draw with a vector font.
text :: Monoid a => String -> Picture a
text = wrap . Text

-- | A bitmap image with a width, height and a Vector holding the
--   32-bit RGBA bitmap data.
--
--  The boolean flag controls whether Gloss should cache the data
--  between frames for speed.
--  If you are programatically generating the image for
--  each frame then use `False`.
--  If you have loaded it from a file then use `True`.
bitmap  :: Monoid a => Int -> Int -> BitmapData -> Bool -> Picture a
bitmap = (((wrap.).).) . Bitmap

-- | A picture drawn with this color.
color :: Monoid a => Color -> Picture a -> Picture a
color = (wrap.) . Color

-- | A picture translated by the given x and y coordinates.
translate :: Monoid a => Float -> Float -> Picture a -> Picture a
translate = ((wrap.).) . Translate

-- | A picture rotated clockwise by the given angle (in degrees).
rotate  :: Monoid a => Float -> Picture a -> Picture a
rotate = (wrap.) . Rotate

-- | A picture scaled by the given x and y factors.
scale   :: Monoid a => Float -> Float -> Picture a -> Picture a
scale = ((wrap.).) . Scale

-- | A picture consisting of several others.
pictures :: Monoid a => [Picture a] -> Picture a
pictures = wrap . Pictures
