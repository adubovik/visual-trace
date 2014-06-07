{-# language
   DeriveDataTypeable
 , RecordWildCards
 , TypeFamilies
 #-}

module VisualTrace.Protocol.Image.CachedImage
 ( CachedImage
 ) where

import Control.Monad.Identity
import Data.Typeable(Typeable)
import Data.IORef
import System.IO.Unsafe
import System.Mem.StableName

import qualified Graphics.Gloss as G

import VisualTrace.Protocol.Image
import VisualTrace.Data.Picture
import VisualTrace.Data.Feedback.FeedbackStorage

type Cache b a = IORef [(StableName a, b)]

data CachedImage a = CachedImage
  { imageCurrent :: !a
  , cachePicG :: !(Cache PictureG a)
  , cachePicL :: !(Cache PictureL a)
  , cacheGPic :: !(Cache G.Picture a)
  , cacheFds  :: !(Cache FeedbackStorage a)
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

onCurrectImage :: (a -> a) -> CachedImage a -> CachedImage a
onCurrectImage f = runIdentity . onCurrectImageM (return . f)

onCurrectImageM :: Monad m => (a -> m a) -> CachedImage a -> m (CachedImage a)
onCurrectImageM f image@CachedImage{..} = do
  imageCurrent' <- f imageCurrent
  return $ image { imageCurrent = imageCurrent' }

instance Image a => Image (CachedImage a) where
  type Command (CachedImage a) = Command a

  initImage = unsafePerformIO $ do
    cPicG <- newIORef []
    cPicL <- newIORef []
    cGPic <- newIORef []
    cFds  <- newIORef []
    return $ CachedImage { imageCurrent = initImage
                          , cachePicG = cPicG
                          , cachePicL = cPicL
                          , cacheGPic = cGPic
                          , cacheFds  = cFds
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

  getFeedbackStorage viewPort image@CachedImage{..} =
    cache imageCurrent cacheFds (stdGetFeedbackStorage viewPort image)

  showImage image = "CachedImage:\n" ++ showImage (imageCurrent image)
