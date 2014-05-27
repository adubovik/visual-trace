{-# language
   TypeFamilies
 , FlexibleContexts
 , DeriveDataTypeable
 , RecordWildCards
 , CPP
 #-}

module VisualTrace.Protocol.Image
 ( Image(..)
 , CachedImage
 , interpretCommand
 ) where

import System.IO.Unsafe
import System.Mem.StableName
import Data.Typeable(Typeable, Typeable1, cast)
import Data.IORef

import qualified Graphics.Gloss as G
import Graphics.Gloss.Data.ViewPort

import VisualTrace.Data.PictureF.Trans
import VisualTrace.Data.PictureF

class (Typeable a, Read (Command a)) => Image a where
  type Command a :: *

  initImage :: a
  drawImageG :: a -> PictureG
  evolveImage :: Float -> a -> a
  interpret :: Command a -> a -> a

  drawImage :: ViewPort -> a -> PictureL
  drawImage = stdDrawImage

  draw :: ViewPort -> a -> G.Picture
  draw = stdDraw

  onBaseImage :: (Monad m, Typeable b, Typeable1 m) =>
                 (b -> m b) -> (a -> m a)
  onBaseImage f a = case cast f of
    Just f' -> f' a
    Nothing -> return a

stdDrawImage :: Image a => ViewPort -> a -> PictureL
stdDrawImage viewPort = desugarePicture viewPort . drawImageG

stdDraw :: Image a => ViewPort -> a -> G.Picture
stdDraw viewPort = toPicture . flattenPicture . drawImage viewPort

type Cache b a = IORef [(StableName a, b)]

data CachedImage a = CachedImage
  { imageCurrent :: !a
  , cachePicG :: !(Cache PictureG a)
  , cachePicL :: !(Cache PictureL a)
  , cacheGPic :: !(Cache G.Picture a)
  }
  deriving Typeable

cache :: Image a => a -> Cache b a -> b -> b
cache image cacheRef newVal = unsafePerformIO $ do
  name <- makeStableName image
  cacheDict <- readIORef cacheRef
  case lookup name cacheDict of
    Nothing -> do
      pushNewVal cacheRef (name, newVal)
      return newVal
    Just oldVal -> do
      return oldVal
  where
    pushNewVal :: Cache b a -> (StableName a, b) -> IO ()
    pushNewVal ref val = modifyIORef ref (take cacheLimit . (val :))

    cacheLimit :: Int
    cacheLimit = 1

-- FIXME: INLINE doesn't work o_O.
-- Creates same IORef for each call of `_initCache`
-- even if `b` type variable is instantiated to different types.
_initCache :: Cache b a
_initCache = unsafePerformIO $ newIORef []
{-# INLINE _initCache #-}

#define initCache (unsafePerformIO $ newIORef [])

onCurrectImage :: (a -> a) -> CachedImage a -> CachedImage a
onCurrectImage f im = im { imageCurrent = f (imageCurrent im) }

onCurrectImageM :: Monad m => (a -> m a) -> CachedImage a -> m (CachedImage a)
onCurrectImageM f image@CachedImage{..} = do
  imageCurrent' <- f imageCurrent
  return $ image { imageCurrent = imageCurrent' }

instance Image a => Image (CachedImage a) where
  type Command (CachedImage a) = Command a

  initImage = CachedImage { imageCurrent = initImage
                          , cachePicG = initCache
                          , cachePicL = initCache
                          , cacheGPic = initCache
                          }

  evolveImage secElapsed = onCurrectImage (evolveImage secElapsed)
  interpret command      = onCurrectImage (interpret command)

  drawImageG CachedImage{..} =
    cache imageCurrent cachePicG (drawImageG imageCurrent)

  drawImage viewPort image@CachedImage{..} =
    cache imageCurrent cachePicL (stdDrawImage viewPort image)

  draw viewPort image@CachedImage{..} =
    cache imageCurrent cacheGPic (stdDraw viewPort image)

  onBaseImage f a = case cast f of
    Just f' -> onCurrectImageM f' a
    Nothing -> return a

interpretCommand :: Image a => String -> a -> Either String a
interpretCommand command img =
  case reads command of
    [] -> Left $ "Can't read command: " ++ command ++ "."
    (command',_):_ -> Right $ interpret command' img
