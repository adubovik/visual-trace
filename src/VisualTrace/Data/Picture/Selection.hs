module VisualTrace.Data.Picture.Selection (
   select
 , selectWithExt
 , selectWithBorder
 ) where

import qualified Data.Foldable as Foldable
import qualified Data.Traversable as Traversable
import Data.Monoid

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Applicative

import Graphics.Gloss(yellow, Point)

import VisualTrace.Data.Feedback
import VisualTrace.Data.Fix
import VisualTrace.Data.Picture hiding (local)
import VisualTrace.Data.Ext
import VisualTrace.Data.Ext.Utils
import VisualTrace.Data.Matrix

type PictureE = PictureA Float Ext

type FeedbackList = [(ExWrap Feedback, Ext)]

select :: Point -> PictureL -> Maybe PictureL
select = ((fst <$>).). selectWithExt

selectWithExt :: Point -> PictureL -> Maybe (PictureL, Ext)
selectWithExt point picture = selectedPic
  where
    selectedPic =
      getFirst . mconcat .
      map (isTriggerFeedback point) $
      buildFeedbackList picture

    isTriggerFeedback :: Point -> (ExWrap Feedback, Ext) ->
                         First (PictureL, Ext)
    isTriggerFeedback p (fb, ext)
      | pointInExt ext p = First $ Just (wrap $ SelectionTrigger fb blank, ext)
      | otherwise = mempty

-- Returns Feedback list in order from drawn last, to drawn first.
-- So we have to trigger first feedback in the list that contains
-- a given point.
buildFeedbackList :: PictureL -> FeedbackList
buildFeedbackList = reverse . feedbacks . annotateWithExt
  where
    feedbacks :: PictureE -> FeedbackList
    feedbacks = snd . runWriter .
                para gatherFeedbacksAlg

    gatherFeedbacksAlg :: PictureFL (Writer FeedbackList (), PictureE) ->
                          Writer FeedbackList ()
    gatherFeedbacksAlg (SelectionTrigger fb (_,pic)) = do
      tell [(fb, getFixAnnotation pic)]
    gatherFeedbacksAlg pic = do
      Foldable.traverse_ fst pic

annotateWithExt :: PictureL -> PictureE
annotateWithExt = flip runReader mempty .
                  cata annExtAlg
  where
    annExtAlg :: PictureFL (Reader Matrix PictureE) ->
                 Reader Matrix PictureE
    annExtAlg picture = do
      matrix <- ask
      -- picture' :: PictureFL PictureE
      picture' <- local (<> getMatrix picture) $
                    Traversable.sequence picture

      let extAlgPassive :: PictureFL Ext -> Ext
          extAlgPassive (Translate _ _ e) = e
          extAlgPassive (Scale     _ _ e) = e
          extAlgPassive pe = extAlg pe

      let picExt = extAlgPassive $ fmap getFixAnnotation picture'
          picExt' | isAtomPic picture' = applyMatrixToExt matrix picExt
                  | otherwise          = picExt

      return $ Fix (picExt', picture')

selectWithBorder :: Point -> PictureL -> PictureL
selectWithBorder point picture = picture'
  where
    selectedPic = selectWithExt point picture

    picture' = case selectedPic of
      Nothing -> picture
      Just (pic,ext) -> pictures [ picture
                                 , color yellow $ drawExt NoFill ext
                                 , pic
                                 ]
