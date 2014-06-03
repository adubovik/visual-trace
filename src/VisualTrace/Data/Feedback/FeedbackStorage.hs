module VisualTrace.Data.Feedback.FeedbackStorage
 ( FeedbackData
 , FeedbackList
 , FeedbackStorage
 , feedbackDataUnderPoint
 , mkFeedbackStorage

 , getFdFeedback
 , getFdExt
 ) where

import Data.Monoid

import Graphics.Gloss(Point)
import VisualTrace.Data.Feedback
import VisualTrace.Data.Ext

data FeedbackData = FeedbackData
  { fdFeedback :: ExWrap Feedback
  , fdExt :: Ext
  }

data FeedbackStorage = FeedbackStorage [FeedbackData]
type FeedbackList    = [(ExWrap Feedback, Ext)]

feedbackDataUnderPoint :: FeedbackStorage -> Point -> Maybe FeedbackData
feedbackDataUnderPoint (FeedbackStorage feedbacks) point =
  getFirst . mconcat . map (isTriggerFeedback point) $ feedbacks
  where
    isTriggerFeedback :: Point -> FeedbackData -> First FeedbackData
    isTriggerFeedback p fbd
      | pointInExt (fdExt fbd) p = First $ Just fbd
      | otherwise = mempty

mkFeedbackStorage :: FeedbackList -> FeedbackStorage
mkFeedbackStorage = FeedbackStorage . fmap (uncurry FeedbackData)

getFdFeedback :: FeedbackData -> ExWrap Feedback
getFdFeedback = fdFeedback

getFdExt :: FeedbackData -> Ext
getFdExt = fdExt

