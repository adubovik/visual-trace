{-# language
   ExistentialQuantification
 , FlexibleInstances
 , DeriveDataTypeable
 #-}

module Graphics.Gloss.Data.Feedback
 ( Feedback
 , ComposableFeedback
 , FeedbackId
 , mkFeedback
 , mkCompFeedback
 , runFeedback
 , getFocusCapture

 , noSideEffect
 , traceSideEffect

 , ExWrap(..)
 ) where

import Control.Applicative
import Control.Arrow

import Data.Function
import Data.Typeable
import Data.Monoid
import Text.Printf

import Graphics.Gloss.Data.EventInfo(EventInfo(..), FocusCapture)

type FeedbackId = String

data ExWrap f = forall a. Typeable a => ExWrap { unExWrap :: f a }

instance Show (ExWrap Feedback) where
  show (ExWrap a) = show a

instance Eq (ExWrap Feedback) where
  (ExWrap a) == (ExWrap b) = maybe False (==b) $ cast a

data Feedback a = Feedback
  { fbFocusCapture :: EventInfo -> FocusCapture
  , fbId           :: FeedbackId
  , fbComposable   :: ComposableFeedback a
  }
  deriving Typeable

data ComposableFeedback a = ComposableFeedback
  { fbSideEffect :: FocusCapture -> EventInfo -> a -> IO ()
  , fbTransform  :: FocusCapture -> EventInfo -> a -> a
  }

noSideEffect :: FocusCapture -> EventInfo -> a -> IO ()
noSideEffect _ _ _ = return ()

traceSideEffect :: FeedbackId -> FocusCapture -> EventInfo -> a -> IO ()
traceSideEffect feedbackId focusCapture eventInfo _ =
  putStrLn $ printf "FocusCapture = %s\n\
                    \EventInfo    = %s\n\
                    \FeedbackId   = %s"
                (show focusCapture)
                (show eventInfo)
                (show feedbackId)


runFeedback :: Feedback a -> EventInfo -> (a -> IO a)
runFeedback fb ef a = do
  let focusCapture = fbFocusCapture fb ef
  fbSideEffect (fbComposable fb) focusCapture ef a
  return $ fbTransform (fbComposable fb) focusCapture ef a

getFocusCapture :: Feedback a -> EventInfo -> FocusCapture
getFocusCapture = fbFocusCapture

mkCompFeedback :: (FocusCapture -> EventInfo -> a -> IO ()) ->
                  (FocusCapture -> EventInfo -> a -> a) ->
                  ComposableFeedback a
mkCompFeedback = ComposableFeedback

mkFeedback :: (EventInfo -> FocusCapture) ->
              FeedbackId ->
              ComposableFeedback a ->
              Feedback a
mkFeedback = Feedback

-- Behave like (.), ie in `x <> y` y will work out first.
instance Monoid (ComposableFeedback a) where
  mempty =
    ComposableFeedback { fbSideEffect = \_ _ _ -> return ()
                       , fbTransform  = \_ _   -> id
                       }
  x `mappend` y =
    ComposableFeedback { fbSideEffect =  \f e ->
                           fbSideEffect y f e >*>
                           fbSideEffect x f e
                       , fbTransform  =  \f e ->
                           fbTransform  y f e >>>
                           fbTransform  x f e
                       }
    where
      (>*>) = liftA2 (>>)

instance Show (Feedback a) where
  show fb = printf "Feedback (%s)" (fbId fb)

instance Eq (Feedback a) where
  (==) = (==) `on` fbId
