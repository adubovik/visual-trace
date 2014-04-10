{-# language
   TypeOperators
 , ScopedTypeVariables
 , ViewPatterns
 #-}

module Graphics.Gloss.Data.PictureF.Selection (
   annotationUnderPoint
 , select
 , selectWithExt
 ) where

import qualified Data.Foldable as Foldable
import qualified Data.Traversable as Traversable
import Data.Fix
import Data.Monoid

import Control.Monad.State
import Control.Applicative
import Control.Applicative.WrapMonadDual

import Graphics.Gloss(yellow)
import Graphics.Gloss.Data.PictureF hiding (ann)
import Graphics.Gloss.Data.ExtentF
import Graphics.Gloss.Utils
import Graphics.Gloss(Point)

annotationUnderPoint :: Point -> Picture (Maybe a) -> Maybe a
annotationUnderPoint point = getFirst . annotationUnderPoint' point . toFirst
  where
    toFirst :: Picture (Maybe a) -> Picture (First a)
    toFirst = cata (Fix . onAnn First)

annotationUnderPoint' :: forall ann . Monoid ann => Point -> Picture ann -> ann
annotationUnderPoint' point = maybe mempty id . getFirst .
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

selectWithExt :: forall ann. Monoid ann =>
                 Point -> Picture ann -> Picture ann
selectWithExt point = select point extBorder
  where
    extBorder :: Picture ann -> Picture ann
    extBorder pic = let ext = getPictureExtP pic
                    in pictures [ color yellow
                                $ fromPicture
                                $ drawExt
                                $ enlargeExt 1.05 1.05
                                $ ext
                                , pic
                                ]

select :: forall ann . Point ->
          (Picture ann -> Picture ann) ->
           Picture ann -> Picture ann
select point selectionTrans = flip evalState False
                             . cataCtx calcPoint alg point
  where
    calcPoint :: Point -> (K ann :*: PictureF) () -> Point
    calcPoint pt pic = invertTransform (deAnn pic) pt

    alg :: Point -> (K ann :*: PictureF) (State Bool (Picture ann)) ->
                                         (State Bool (Picture ann))
    alg pt picture@(deAnn -> pic) = do
      -- The last primitive should be processed first, because
      -- it's draw later, ie. it's more visible, then others.
      picture' <- (Fix <$>) .
                  unwrapMonadDual .
                  Traversable.sequenceA .
                  (WrapMonadDual <$>) $
                  picture
      wasMatch <- get
      if wasMatch
      then return picture'
      else do
        let isCurrentMatch = pointInAtomPicture pic pt
            transform | isCurrentMatch = selectionTrans
                      | otherwise = id
        put isCurrentMatch
        return $ transform picture'

pointInAtomPicture :: PictureF a -> Point -> Bool
pointInAtomPicture = pointInExt . getBasicExt

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
invertTransform Rotate{}          = error "invertTransform: Rotate isn't yet supported."
invertTransform (Scale x y _)     = \(a,b) -> (a/x, b/y)
invertTransform Pictures{}        = id

