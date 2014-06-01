{-# language
   DeriveDataTypeable
 , RecordWildCards
 , TypeFamilies
 , CPP
 #-}

module VisualTrace.Protocol.Image.CachedImage
 ( CachedImage
 ) where

import Data.Typeable(Typeable)
import System.IO.Unsafe
import System.Mem.StableName
import Data.IORef

import VisualTrace.Protocol.Image

import qualified Graphics.Gloss as G
import VisualTrace.Data.PictureF

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
  onBaseImage f          = onCurrectImageM (onBaseImage f)

  drawImageG CachedImage{..} =
    cache imageCurrent cachePicG (drawImageG imageCurrent)

  drawImage viewPort image@CachedImage{..} =
    cache imageCurrent cachePicL (stdDrawImage viewPort image)

  draw viewPort image@CachedImage{..} =
    cache imageCurrent cacheGPic (stdDraw viewPort image)

