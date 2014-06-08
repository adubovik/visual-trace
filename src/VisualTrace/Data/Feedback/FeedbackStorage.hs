module VisualTrace.Data.Feedback.FeedbackStorage
 ( FeedbackData
 , FeedbackList
 , FeedbackStorage
 , feedbackDataUnderPoint
 , mkFeedbackStorage

 , getFdFeedback
 , getFdExt
 ) where

import qualified Data.Map as Map

import Graphics.Gloss(Point)
import VisualTrace.Data.Feedback
import VisualTrace.Data.Ext
import VisualTrace.Data.SegmentTree

data FeedbackData = FeedbackData
  { fdFeedback :: ExWrap Feedback
  , fdExt :: Ext
  }

type ZIndex = Int
type FeedbackStorage = SegmentTree2D Float (Map.Map ZIndex FeedbackData)
type FeedbackList    = [(ExWrap Feedback, Ext)]

feedbackDataUnderPoint :: FeedbackStorage -> Point -> Maybe FeedbackData
feedbackDataUnderPoint storage point =
  let dataUnderPoint = querySegmentTree2D storage point
  in  case Map.null dataUnderPoint of
    True  -> Nothing
    False -> Just $ snd $ Map.findMin dataUnderPoint

mkFeedbackStorage :: FeedbackList -> FeedbackStorage
mkFeedbackStorage feedBackList =
  let feedBackListWithZ = feedBackList `zip` [1..]
  in  mkSegmentTree2D $
      [ ( ( (xm,xM)
          , (ym,yM)
          )
        , Map.singleton zIdx (FeedbackData wfeedback ext)
        )
      | ((wfeedback, ext@(Ext (Just (ExtentF yM ym xM xm)))),zIdx)
      <- feedBackListWithZ
      ]

getFdFeedback :: FeedbackData -> ExWrap Feedback
getFdFeedback = fdFeedback

getFdExt :: FeedbackData -> Ext
getFdExt = fdExt

