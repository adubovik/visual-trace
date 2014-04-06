{-# language
   TypeOperators
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

select :: Monoid ann => (Float, Float) -> Picture ann -> ann
select p = flip runReader (p,mempty) . cata selectAlg
  where
    selectAlg :: Monoid ann =>
                 (K ann :*: PictureF) (Reader ((Float,Float),ann) ann) ->
                                       Reader ((Float,Float),ann) ann
    selectAlg (K ann :*: f) = do
      (p,ann') <- ask
      let ann'' = ann <> ann'
          p' = invertTransform f p
      local (const (p',ann'')) $ do
        fann <- Traversable.sequence f
        let annMatch | pointInPicture fann p' = ann''
                     | otherwise = mempty
        return $
          -- TODO: non-trivial list will be only in case of
          -- Pictures primitive. We want to grab only first match,
          -- because the later ones will be drawn underneath the first.
          mconcat (Foldable.toList fann) <> annMatch

pointInPicture :: PictureF a -> (Float,Float) -> Bool
pointInPicture pic = pointInExt (getBasicExt pic)

invertTransform :: PictureF a -> (Float,Float) -> (Float,Float)
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

