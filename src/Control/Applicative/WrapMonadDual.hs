module Control.Applicative.WrapMonadDual where

import Control.Applicative
import Control.Monad

newtype WrappedMonadDual m a = WrapMonadDual { unwrapMonadDual :: m a }

instance Monad m => Functor (WrappedMonadDual m) where
        fmap f (WrapMonadDual v) = WrapMonadDual (liftM f v)

instance Monad m => Applicative (WrappedMonadDual m) where
        pure = WrapMonadDual . return
        WrapMonadDual f <*> WrapMonadDual v = WrapMonadDual $ do
          v' <- v
          f' <- f
          return $ f' v'
