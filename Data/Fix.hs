{-# language
   TypeOperators
 , DeriveFunctor
 #-}

module Data.Fix where

newtype K b a = K { unK :: b }
  deriving Functor

data (f :*: g) a = f a :*: g a
  deriving Functor

newtype Fix f = Fix { unFix :: f (Fix f) }

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

ana :: Functor f => (a -> f a) -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg
