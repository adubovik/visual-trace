{-# language
   TypeFamilies
 , FlexibleContexts
 , DeriveDataTypeable
 , RecordWildCards
 #-}

module VisualTrace.Protocol.Image
 ( Image(..)
 , OptImage
 , interpretCommand
 ) where

import System.IO.Unsafe
import System.Mem.StableName
import Data.IORef
import Data.Typeable(Typeable)
import Control.Applicative

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
  drawImage viewPort = desugarePicture viewPort . drawImageG

  draw :: ViewPort -> a -> G.Picture
  draw viewPort = toPicture . flattenPicture . drawImage viewPort

type Pictures = (PictureG, Maybe PictureL, Maybe G.Picture)
type PictureStorage a = IORef [(StableName a, Pictures)]

storeAndLookup :: Image a => PictureStorage a -> Maybe ViewPort -> a -> Pictures
storeAndLookup storageRef optViewPort image = unsafePerformIO $ do
  name <- makeStableName image
  storage <- readIORef storageRef
  case lookup name storage of
    Nothing -> do
      let picG = drawImageG image
          picL = flip desugarePicture picG <$> optViewPort
          gpic = toPicture . flattenPicture <$> picL
          pics = (picG,picL,gpic)
      pushNewPic storageRef (name,pics)
      return pics
    Just pics@(_,Just _,Just _) ->
      return pics
    Just (picG,_,_) -> do
      let picL = flip desugarePicture picG <$> optViewPort
          gpic = toPicture . flattenPicture <$> picL
          pics = (picG,picL,gpic)
      pushNewPic storageRef (name,pics)
      return pics
  where
    pushNewPic :: PictureStorage a -> (StableName a, Pictures) -> IO ()
    pushNewPic st pics = modifyIORef st (take storageLimit . (pics :))

    storageLimit :: Int
    storageLimit = 1

data OptImage a = OptImage
  { imageCurrent   :: !a
  , pictureStorage :: !(PictureStorage a)
  }
  deriving Typeable

onCurrectImage :: (a -> a) -> OptImage a -> OptImage a
onCurrectImage f im = im { imageCurrent = f (imageCurrent im) }

instance Image a => Image (OptImage a) where
  type Command (OptImage a) = Command a

  initImage = OptImage { imageCurrent   = initImage
                       , pictureStorage = initStorage
                       }
    where
      initStorage :: PictureStorage a
      initStorage = unsafePerformIO $ newIORef []

  evolveImage secElapsed = onCurrectImage (evolveImage secElapsed)
  interpret command      = onCurrectImage (interpret command)

  drawImageG OptImage{..} =
    let (picG,_,_) = storeAndLookup pictureStorage Nothing imageCurrent
    in picG

  drawImage viewPort OptImage{..} =
    let (_,Just picL,_) =
          storeAndLookup pictureStorage (Just viewPort) imageCurrent
    in picL

  draw viewPort OptImage{..} =
    let (_,_,Just gpic) =
          storeAndLookup pictureStorage (Just viewPort) imageCurrent
    in gpic

interpretCommand :: Image a => String -> a -> Either String a
interpretCommand command img =
  case reads command of
    [] -> Left $ "Can't read command: " ++ command ++ "."
    (command',_):_ -> Right $ interpret command' img
