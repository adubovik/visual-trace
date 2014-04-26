{-# language
   DeriveFunctor
 , DeriveFoldable
 , DeriveTraversable
 , DeriveDataTypeable
 , TypeOperators
 , NoMonomorphismRestriction
 , ViewPatterns
 , TupleSections
 , ExistentialQuantification
 #-}

module Graphics.Gloss.Data.PictureF
 ( PictureF(..)
 , Feedback(..)
 , ExWrap(..)
 , Picture
 , PictureA
 , GroupId
 , Annotation

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
 , group
 , annotate
 , selectionTrigger

 -- Misc
 , getMatrix
 , wrap
 , unWrap
 ) where

import Graphics.Gloss(Path, BitmapData, Color)
import Graphics.Gloss.Data.Matrix
import Graphics.Gloss.Data.Event(Event)

import Data.Foldable(Foldable)
import Data.Traversable(Traversable)
import Data.Fix
import Data.Monoid
import Data.Typeable

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
  | SelectionTrigger (ExWrap Feedback) a
  deriving (Functor, Traversable, Foldable, Eq, Show)

data ExWrap f = forall a. Typeable a => ExWrap { unExWrap :: f a }

instance Show (ExWrap f) where
  show _ = "(ExWrap)"

instance Eq (ExWrap a) where
  (==) = const $ const True

data Feedback a = Feedback
  { fbSideEffect :: Event -> a -> IO ()
  , fbTransform  :: Event -> a -> a
  }
  deriving Typeable

instance Show (Feedback a) where
  show _ = "(Feedback)"

instance Eq (Feedback a) where
  (==) = const $ const True

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

group :: GroupId -> Picture -> Picture
group = (wrap.) . Group

annotate :: Annotation -> Picture -> Picture
annotate = (wrap.) . Annotate

selectionTrigger :: ExWrap Feedback -> Picture -> Picture
selectionTrigger = (wrap.) . SelectionTrigger
