{-# language
   TypeOperators
 , DeriveFunctor
 , RankNTypes
 , ScopedTypeVariables
 , TupleSections
 , DeriveTraversable
 , DeriveFoldable
 , StandaloneDeriving
 , FlexibleContexts
 , UndecidableInstances
 #-}

module Data.Fix where

import Control.Applicative((<$>))
import Control.Arrow
import Data.Traversable
import Data.Foldable

newtype K b a = K { unK :: b }
  deriving (Show, Functor, Traversable, Foldable)

data (f :*: g) a = f a :*: g a
  deriving (Show, Functor, Traversable, Foldable)

newtype Fix t f = Fix { unFix :: (t, f (Fix t f)) }

deriving instance (Show t, Show (f (Fix t f))) => Show (Fix t f)

cata :: Functor f => (f a -> a) -> Fix x f -> a
cata alg = alg . fmap (cata alg) . snd . unFix

cataWithAnnotation :: Functor f =>
                      (x -> f a -> a) -> Fix x f -> a
cataWithAnnotation alg =
  uncurry alg .
  (id *** fmap (cataWithAnnotation alg)) .
  unFix

para :: Functor f => (f (a, Fix x f) -> a) -> Fix x f -> a
para alg = alg . fmap (para alg &&& id) . snd . unFix

paraWithAnnotation :: Functor f => (x -> f (a, Fix x f) -> a) -> Fix x f -> a
paraWithAnnotation alg =
  uncurry alg .
  (id *** fmap (paraWithAnnotation alg &&& id)) .
  unFix

ana :: Functor f => (a -> f a) -> a -> Fix () f
ana coalg = Fix . ((),) . fmap (ana coalg) . coalg

annotateCata :: forall f a b. Functor f =>
                ((b, f a) -> a) -> Fix b f -> Fix a f
annotateCata alg = Fix .
                   (getA &&& snd) .
                   second (fmap $ annotateCata alg) .
                   unFix
  where
    getA :: Functor f => (b, f (Fix a f)) -> a
    getA = alg . second (fmap $ fst . unFix)

annotateAna :: Functor f =>
               ((a, (b, f (Fix b f))) -> f (a, Fix b f)) ->
                (a, Fix b f) -> Fix a f
annotateAna coalg = Fix .
                    second (fmap $ annotateAna coalg) .
                    (fst &&& coalg) .
                    second unFix

-- cata with up-down context
cataCtx' :: Functor f =>
           (forall b . ctx -> f b -> f (ctx, b)) ->
           (ctx -> f a -> a) ->
           ctx -> Fix x f -> a
cataCtx' iterCtx alg ctx =
  alg ctx . fmap (uncurry $ cataCtx' iterCtx alg) . iterCtx ctx . snd . unFix

cataCtx :: forall ctx f a x. Functor f =>
           (ctx -> f () -> ctx) ->
           (ctx -> f a -> a) ->
           ctx -> Fix x f -> a
cataCtx iterCtx alg ctx =
  alg ctx . fmap (uncurry $ cataCtx iterCtx alg) . propagateCtx ctx . snd . unFix
  where
    propagateCtx :: forall b. ctx -> f b -> f (ctx, b)
    propagateCtx c f = (iterCtx c (const () <$> f),) <$> f
