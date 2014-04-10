{-# language
   TypeOperators
 , DeriveFunctor
 , RankNTypes
 , ScopedTypeVariables
 , TupleSections
 , DeriveTraversable
 , DeriveFoldable
 #-}

module Data.Fix where

import Control.Applicative((<$>))
import Data.Traversable
import Data.Foldable

newtype K b a = K { unK :: b }
  deriving (Functor, Traversable, Foldable)

data (f :*: g) a = f a :*: g a
  deriving (Functor, Traversable, Foldable)

newtype Fix f = Fix { unFix :: f (Fix f) }

onceBottomUp :: Functor f => (f (Fix g) -> Fix g) -> Fix f -> Fix g
onceBottomUp trans = cata trans

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

-- cata with up-down context
cataCtx' :: Functor f =>
           (forall a . ctx -> f a -> f (ctx, a)) ->
           (ctx -> f a -> a) ->
           ctx -> Fix f -> a
cataCtx' iterCtx alg ctx =
  alg ctx . fmap (uncurry $ cataCtx' iterCtx alg) . iterCtx ctx . unFix

cataCtx :: forall ctx f a. Functor f =>
           (ctx -> f () -> ctx) ->
           (ctx -> f a -> a) ->
           ctx -> Fix f -> a
cataCtx iterCtx alg ctx =
  alg ctx . fmap (uncurry $ cataCtx iterCtx alg) . propagateCtx ctx . unFix
  where
    propagateCtx :: forall a. ctx -> f a -> f (ctx, a)
    propagateCtx c f = (iterCtx c (const () <$> f),) <$> f


ana :: Functor f => (a -> f a) -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg
