{-# language
   TypeOperators
 , ScopedTypeVariables
 #-}

module Graphics.Gloss.Data.PictureF.Selection (
  select
 ) where

import Data.Foldable(Foldable)
import qualified Data.Foldable as Foldable
import Data.Traversable(Traversable)
import qualified Data.Traversable as Traversable
import Data.Fix
import Data.Monoid

import Control.Monad.Reader

import Graphics.Gloss.Data.PictureF
import Graphics.Gloss.Data.ExtentF
import Graphics.Gloss.Utils
import Graphics.Gloss(Point)

select :: Point -> Picture (Maybe a) -> Maybe a
select point = getFirst . select' point . toFirst
  where
    toFirst :: Picture (Maybe a) -> Picture (First a)
    toFirst = cata (Fix . onAnn First)

select' :: forall ann . Monoid ann => Point -> Picture ann -> ann
select' point = maybe mempty id . getFirst .
                cataCtx calcAnn alg (point, mempty)
  where
    calcAnn :: (Point, ann) -> (K ann :*: PictureF) () -> (Point, ann)
    calcAnn (pt, ann) (K primAnn :*: pic) =
      let ann' = primAnn <> ann
          pt'  = invertTransform pic pt
      in  (pt', ann')

    alg :: (Point, ann) -> (K ann :*: PictureF) (First ann) -> First ann
    alg (pt, ann) (K primAnn :*: pic) =
      let annMatch = First . Just $ primAnn <> ann
          -- NB: non-trivial list will be only in the case of
          -- Pictures primitive. We want to grab only last match,
          -- because the former ones will be drawn underneath the last.
          annKids = reverse $ Foldable.toList pic
          annRes | pointInAtomPicture pic pt = annMatch
                 | otherwise = mconcat annKids
      in  annRes

pointInAtomPicture :: PictureF a -> Point -> Bool
pointInAtomPicture pic = pointInExt (getBasicExt pic)

invertTransform :: PictureF a -> Point -> Point
invertTransform Blank             = id
invertTransform Polygon{}         = id
invertTransform Line{}            = id
invertTransform Circle{}          = id
invertTransform ThickCircle{}     = id
invertTransform Arc{}             = id
invertTransform ThickArc{}        = id
invertTransform Text{}            = id
invertTransform Bitmap{}          = id
invertTransform Color{}           = id
invertTransform (Translate x y _) = \(a,b) -> (a-x,b-y)
invertTransform (Rotate a b)      = error "invertTransform: Rotate isn't yet supported."
invertTransform (Scale x y _)     = \(a,b) -> (a/x, b/y)
invertTransform Pictures{}        = id

