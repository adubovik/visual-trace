{-# language
   DeriveFunctor
 , DeriveFoldable
 , DeriveTraversable
 , TypeOperators
 , NoMonomorphismRestriction
 , ViewPatterns
 , TupleSections
 #-}

module Graphics.Gloss.Data.PictureF
 ( PictureF(..)
 , Picture
 , PictureA
 , GroupId
 , Annotation
 , fromPicture
 , toPicture

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

 -- PictureF specific
 , fixHeight
 , fixWidth
 , group
 , annotate

 , getMatrix
 ) where

import qualified Graphics.Gloss as G
import Graphics.Gloss(Path, BitmapData, Color)
import Graphics.Gloss.Data.Matrix

import Data.Foldable(Foldable)
import Data.Traversable(Traversable)
import Data.Fix
import Data.Monoid

type GroupId = Int
type Annotation = String

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

  -- PictureF specific cases
  | FixedSize (Maybe Float) (Maybe Float) a
  | Group GroupId a
  | Annotate Annotation a
  deriving (Functor, Traversable, Foldable, Show, Eq)

type PictureA a = Fix a PictureF
type Picture    = PictureA ()

toPicture :: Picture -> G.Picture
toPicture = cataCtx iterateMatrix alg identityMatrix
  where
    identityMatrix :: Matrix
    identityMatrix = mempty

    iterateMatrix :: Matrix -> PictureF () -> Matrix
    iterateMatrix m pic = m <> getMatrix pic

    alg :: Matrix -> PictureF G.Picture -> G.Picture
    alg _m pic = case pic of
      Blank                -> G.Blank
      Polygon p            -> G.Polygon p
      Line p               -> G.Line p
      Circle a             -> G.Circle a
      ThickCircle a b      -> G.ThickCircle a b
      Arc a b c            -> G.Arc a b c
      ThickArc a b c d     -> G.ThickArc a b c d
      Text s               -> G.Text s
      Bitmap a b c d       -> G.Bitmap a b c d
      Color c a            -> G.Color c a
      Translate a b c      -> G.Translate a b c
      Rotate a b           -> G.Rotate a b
      Scale a b c          -> G.Scale a b c
      Pictures p           -> G.Pictures p
      FixedSize _mw _mh p  -> p -- let ext = getPictureExt p
      Group _ p            -> p
      Annotate _ p         -> p

getMatrix :: PictureF a -> Matrix
getMatrix pic = case pic of
  Translate x y _ -> identityTranslate (x,y)
  Scale x y _     -> identityScale (x,y)
  Rotate{}        -> error "getMatrix: Rotate isn't yet supported."
  _               -> mempty

fromPicture :: G.Picture -> Picture
fromPicture = ana coalg
  where
    coalg pic = case pic of
      G.Blank            -> Blank
      G.Polygon p        -> Polygon p
      G.Line p           -> Line p
      G.Circle a         -> Circle a
      G.ThickCircle a b  -> ThickCircle a b
      G.Arc a b c        -> Arc a b c
      G.ThickArc a b c d -> ThickArc a b c d
      G.Text s           -> Text s
      G.Bitmap a b c d   -> Bitmap a b c d
      G.Color c a        -> Color c a
      G.Translate a b c  -> Translate a b c
      G.Rotate a b       -> Rotate a b
      G.Scale a b c      -> Scale a b c
      G.Pictures p       -> Pictures p

wrap :: PictureF Picture -> Picture
wrap = Fix . ((),)

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

-- | Fix absolute height of the picture (in pixels)
fixHeight :: Float -> Picture -> Picture
fixHeight h = wrap . FixedSize Nothing (Just h)

-- | Fix absolute width of the picture (in pixels)
fixWidth :: Float -> Picture -> Picture
fixWidth w = wrap . FixedSize (Just w) Nothing

group :: GroupId -> Picture -> Picture
group = (wrap.) . Group

annotate :: Annotation -> Picture -> Picture
annotate = (wrap.) . Annotate
