{-# language
   DeriveFunctor
 , DeriveFoldable
 , DeriveTraversable
 , TupleSections
 #-}

module VisualTrace.Data.Picture
 ( PictureF(..)
 , Picture
 , PictureA

 , PictureG
 , PictureL

 , PictureFG
 , PictureFL

 , Filling(..)
 , GFloat(..)
 , GPath
 , GPoint

 , toPictureG
 , toLocalPoint

 , screen
 , local

 -- Smart constructors
 , blank
 , polygon
 , line
 , circle
 , thickCircle
 , arc
 , thickArc
 , text
 , color
 , translate
 , scale
 , scale'
 , pictures

 -- PictureF specific primitives
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

import Graphics.Gloss.Data.Color
import VisualTrace.Data.Matrix
import VisualTrace.Data.Feedback

import Data.Typeable(Typeable)
import Data.Foldable(Foldable)
import Data.Traversable(Traversable)
import VisualTrace.Data.Fix
import Data.Monoid

import Control.Arrow((***))

data Filling = Fill
             | NoFill
  deriving (Eq, Show)

type GPath d = [(d,d)]
type GPoint d = (d,d)

data PictureF double a
  -- Primitives
  = Blank
  | Text (GPoint double) String
  | Line Filling (GPath double)
  | Arc  (Maybe Float) (Maybe (Float,Float)) (GPoint double) double

  -- Transformations
  | Translate double double a
  | Scale     double double a
  | Color Color a

  -- Composition
  | Pictures [a]

  -- PictureF specific cases
  | SelectionTrigger (ExWrap Feedback) a
  | InsideRect double Filling (Maybe Color) a
  | VCat double [a]
  | HCat double [a]
  deriving (Functor, Traversable, Foldable, Eq, Show)

data GFloat = Screen Float
            | Local  Float
  deriving (Eq, Ord, Show)

screen,local :: Float -> GFloat
screen = Screen
local = Local

type PictureFG = PictureF GFloat
type PictureFL = PictureF  Float

type PictureA d a = Fix a (PictureF d)
type Picture d = PictureA d ()
type PictureG = Picture GFloat
type PictureL = Picture  Float

getMatrix :: PictureF Float a -> Matrix
getMatrix pic = case pic of
  Translate x y _ -> identityTranslate (x,y)
  Scale     x y _ -> identityScale     (x,y)
  _               -> mempty

wrap :: PictureF d (Picture d) -> Picture d
wrap = Fix . ((),)

unWrap :: Picture d -> PictureF d (Picture d)
unWrap = snd . unFix

---------------------
-- Smart constructors.
-- Copied from Graphics.Gloss.Data.Picture.

-- | A blank picture, with nothing in it.
blank :: Picture d
blank = wrap Blank

-- | A convex polygon filled with a solid color.
polygon :: GPath d -> Picture d
polygon = wrap . Line Fill

-- | A line along an arbitrary path.
line :: GPath d -> Picture d
line = wrap . Line NoFill

-- | A circle with the given radius.
circle :: GPoint d -> d -> Picture d
circle = (wrap.) . Arc Nothing Nothing

-- | A circle with the given thickness and radius.
--   If the thickness is 0 then this is equivalent to `circle`.
thickCircle :: Float -> GPoint d -> d -> Picture d
thickCircle thickness = (wrap.) . Arc (Just thickness) Nothing

-- | A circular arc drawn counter-clockwise between two angles (in degrees)
--   at the given radius.
arc :: Float -> Float -> GPoint d -> d -> Picture d
arc ang1 ang2 = (wrap.) . Arc Nothing (Just (ang1,ang2))

-- | A circular arc drawn counter-clockwise between two angles (in degrees),
--   with the given radius  and thickness.
--   If the thickness is 0 then this is equivalent to `Arc`.
thickArc :: Float -> Float -> Float -> GPoint d -> d -> Picture d
thickArc ang1 ang2 thickness =
  (wrap.) . Arc (Just thickness) (Just (ang1,ang2))

-- | Some text to draw with a vector font.
text :: GPoint d -> String -> Picture d
text = (wrap.) . Text

-- | A picture drawn with this color.
color :: Color -> Picture d -> Picture d
color = (wrap.) . Color

-- | A picture translated by the given x and y coordinates.
translate :: d -> d -> Picture d -> Picture d
translate = ((wrap.).) . Translate

-- | A picture scaled by the given x and y factors.
scale :: d -> d -> Picture d -> Picture d
scale = ((wrap.).) . Scale

-- | A picture scaled by the given factor in both direction.
scale' :: d -> Picture d -> Picture d
scale' x = scale x x

-- | A picture consisting of several others.
--   First picture in the list will be drawn first.
pictures :: [Picture d] -> Picture d
pictures [] = blank
pictures ps = wrap $ Pictures ps

-- | PictureF specific primitive.
selectionTrigger :: Typeable a => Feedback a -> Picture d -> Picture d
selectionTrigger fb = wrap . SelectionTrigger (ExWrap fb)

vcat :: d -> [Picture d] -> Picture d
vcat _ [] = blank
vcat padding ps = wrap $ VCat padding $ ps

rvcat :: d -> [Picture d] -> Picture d
rvcat padding = vcat padding . reverse

hcat :: d -> [Picture d] -> Picture d
hcat _ [] = blank
hcat padding ps = wrap $ HCat padding ps

insideRect :: d -> Filling -> Maybe Color -> Picture d -> Picture d
insideRect = (((wrap.).).) . InsideRect

toPictureG :: PictureL -> PictureG
toPictureG = cata alg
  where
    alg :: PictureF Float PictureG -> PictureG
    alg = wrap . fmapOnDoubles local

toLocalPoint :: GPoint Float -> GPoint GFloat
toLocalPoint = local *** local

fmapOnDoubles :: (a -> b) -> PictureF a c -> PictureF b c
fmapOnDoubles f pic = case pic of
  Blank -> Blank
  Text pos s -> Text (ff pos) s
  Line fill ds -> Line fill (map ff ds)
  Arc a b pos rad -> Arc a b (ff pos) (f rad)

  Translate x y p -> Translate (f x) (f y) p
  Scale x y p -> Scale (f x) (f y) p
  Color c p -> Color c p

  Pictures ps -> Pictures ps

  SelectionTrigger fb p -> SelectionTrigger fb p
  InsideRect pad fill c p -> InsideRect (f pad) fill c p
  VCat pad ps -> VCat (f pad) ps
  HCat pad ps -> HCat (f pad) ps
  where
    ff = f *** f
