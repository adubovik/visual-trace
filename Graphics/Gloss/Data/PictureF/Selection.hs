{-# language
   ViewPatterns
 , TupleSections
 , TypeOperators
 , ScopedTypeVariables
 , Rank2Types
 , RecordWildCards
 #-}

module Graphics.Gloss.Data.PictureF.Selection (
   annotationUnderPoint
 , select
 , select'
 , selectWithExt
 ) where

import qualified Data.Traversable as Traversable
import Data.Fix
import Data.Monoid
import Data.Maybe

import Control.Monad.State
import Control.Applicative
import Control.Applicative.WrapMonadDual

import Graphics.Gloss(yellow, Point)
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.PictureF
import Graphics.Gloss.Data.Ext2
import Graphics.Gloss.Data.Ext.Utils
import Graphics.Gloss.Data.Matrix

import Text.Printf

annotationUnderPoint :: Point -> Picture -> Maybe Annotation
annotationUnderPoint _ _ = Nothing

-- annotationUnderPoint point = getFirst . annotationUnderPoint' point . toFirst
--   where
--     toFirst :: Picture (Maybe a) -> Picture (First a)
--     toFirst = cata (Fix . onAnn First)

-- annotationUnderPoint' :: forall ann . Monoid ann => Point -> Picture ann -> ann
-- annotationUnderPoint' point = maybe mempty id . getFirst .
--                               cataCtx calcAnn alg (point, mempty)
--   where
--     calcAnn :: (Point, ann) -> (K ann :*: PictureF) () -> (Point, ann)
--     calcAnn (pt, ann) (K primAnn :*: pic) =
--       let ann' = primAnn <> ann
--           pt'  = invertTransform pic pt
--       in  (pt', ann')

--     alg :: (Point, ann) -> (K ann :*: PictureF) (First ann) -> First ann
--     alg (pt, ann) (K primAnn :*: pic) =
--       let annMatch = First . Just $ primAnn <> ann
--           -- NB: non-trivial list will be only in the case of
--           -- Pictures primitive. We want to grab only last match,
--           -- because the former ones will be drawn underneath the last.
--           annKids = reverse $ Foldable.toList pic
--           annRes | pointInAtomPicture pic pt = annMatch
--                  | otherwise = mconcat annKids
--       in  annRes

selectWithExt :: ViewPort -> Point -> Picture -> Picture
selectWithExt viewPort point = select' viewPort point extBorder
  where
    extBorder :: Picture -> Picture
    extBorder pic = let ext2 = getPictureExt2 pic
                    in pictures [ color yellow
                                $ drawExt2 ext2
                                , pic
                                ]

data SState = SState { selExt    :: Maybe Ext2
                     , selMatrix :: Maybe Matrix
                     , selInExt  :: Maybe Bool
                     }

instance Show SState where
  show SState{..} = printf "\n(%s,%s,%s)\n"
                    (maybe "-" show selExt)
                    (maybe "-" show selMatrix)
                    (maybe "-" show selInExt)

initSState :: SState
initSState = SState { selExt    = Nothing
                    , selMatrix = Nothing
                    , selInExt  = Nothing
                    }

select' :: ViewPort -> Point -> (Picture -> Picture) -> (Picture -> Picture)
select' (viewPortToMatrix -> viewPortMatrix) point selectionTrans pic = pic4
  where
    pic0 :: PictureA SState
    pic0 = annotateCata (const initSState) pic

    pic1 :: PictureA SState
    pic1 = annotateCata extAlg pic0
      where
        extAlg :: (SState, PictureF SState) -> SState
        extAlg (oldState, picture) =
          let newExt = ext2Alg <$> Traversable.traverse selExt picture
          in  oldState { selExt = newExt }

    pic2 :: PictureA SState
    pic2 = annotateAna matAlg ( (fst $ unFix pic1) { selMatrix = Just mempty }
                              , pic1
                              )
      where
        matAlg :: (SState, (SState, PictureF (        PictureA SState))) ->
                                    PictureF (SState, PictureA SState)
        matAlg (currState,(_oldState,picture)) =
          let newMat = do
                currMat <- selMatrix currState
                return $ currMat <> getMatrix picture
              amendMat p = let (st,_p') = unFix p
                               st' = st { selMatrix = newMat }
                           in  (st', p)
          in  fmap amendMat picture

    pic3 :: PictureA SState
    pic3 = annotateCata inPicAlg pic2
      where
        inPicAlg :: (SState, PictureF SState) -> SState
        inPicAlg (oldState, _pic) =
          let inExt = do
                ext <- selExt oldState
                -- FIXME: dirty hack
                let ext' = enlargeStrongExt (recip . fst $ mScale viewPortMatrix) ext
                mat <- selMatrix oldState
                let localPoint = applyMatrix (invertMatrix mat) $
                                 point
                    globalPoint = applyMatrix (zeroScale (invertMatrix mat)) $
                                  point
                    isIn = pointInExt2 ext' globalPoint localPoint
                return isIn
          in oldState { selInExt = inExt }

    pic4 :: PictureA ()
    pic4 = paraWithAnnotation transAlg pic3
      where
        transAlg :: SState ->
                    PictureF (PictureA (), PictureA SState) ->
                    PictureA ()
        transAlg currState picture@(split -> (_newPic, oldPic)) =
          let oldPic' = fmap deAnn oldPic in
          Fix . ((),) $
            case selInExt currState of
              Just True -> if isSelectablePic oldPic'
                           then snd . unFix .
                                selectionTrans .
                                Fix . ((),) $
                                oldPic'
                           else switchLastInExt $
                                picture
              _ -> oldPic'

        switchLastInExt :: Traversable.Traversable f =>
                           f (PictureA (), PictureA SState) ->
                           f (PictureA ())
        switchLastInExt f = flip evalState False .
                            unwrapMonadDual .
                            Traversable.sequenceA .
                            (WrapMonadDual <$>) .
                            (act <$>) $
                            f
          where
            act :: (PictureA (), PictureA SState) -> State Bool (PictureA ())
            act (newPic, oldPic) = do
              wasMatch <- get
              let oldPic' = deAnn oldPic
              if wasMatch
              then return oldPic'
              else do
                let inExt = fromMaybe False . selInExt . fst $
                            unFix oldPic
                if inExt
                then do
                  put True
                  return newPic
                else
                  return oldPic'

        deAnn :: PictureA a -> PictureA ()
        deAnn = annotateCata (const ())

split :: Functor f => f (a,b) -> (f a, f b)
split f = (fst <$> f, snd <$> f)

-- Doesn't consider Group primitive.
select :: Point -> (Picture -> Picture) -> (Picture -> Picture)
select point selectionTrans = flip evalState False
                            . snd
                            . cataCtx calcPoint alg point
  where
    calcPoint :: Point -> PictureF () -> Point
    calcPoint pt pic = invertTransform pic pt


    alg :: Point -> PictureF (Ext2, State Bool Picture) ->
                             (Ext2, State Bool Picture)
    alg pt (split -> (picExt, pic)) =
      let picExt' = ext2Alg picExt in (picExt',) $ do
      -- The last primitive should be processed first, because
      -- it's drawn later, ie. it's more visible, then others.
      picture' <- (Fix . ((),) <$>) .
                  unwrapMonadDual .
                  Traversable.sequenceA .
                  (WrapMonadDual <$>) $
                  pic
      wasMatch <- get
      if wasMatch
      then return picture'
      else do
        let isMatch = isSelectablePic pic &&
                      pointInExt2 picExt' point pt
            transform | isMatch   = selectionTrans
                      | otherwise = id
        put isMatch
        return $ transform picture'

    invertTransform :: PictureF a -> Point -> Point
    invertTransform (Translate x y _) = \(a,b) -> (a-x, b-y)
    invertTransform (Scale x y _)     = \(a,b) -> (a/x, b/y)
    invertTransform Rotate{}          = error "invertTransform: Rotate isn't yet supported."
    invertTransform _                 = id

isSelectablePic :: PictureF a -> Bool
isSelectablePic Blank         = True
isSelectablePic Polygon{}     = True
isSelectablePic Line{}        = True
isSelectablePic Circle{}      = True
isSelectablePic Arc{}         = True
isSelectablePic ThickCircle{} = True
isSelectablePic ThickArc{}    = True
isSelectablePic Text{}        = True
isSelectablePic Bitmap{}      = True
isSelectablePic Group{}       = True
isSelectablePic _             = False
