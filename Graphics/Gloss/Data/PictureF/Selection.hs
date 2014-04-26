{-# language
   ViewPatterns
 , TupleSections
 , TypeOperators
 , ScopedTypeVariables
 , Rank2Types
 , RecordWildCards
 #-}

module Graphics.Gloss.Data.PictureF.Selection (
   annotationUnderPoint
 , select
 , selectWithExt
 ) where

import qualified Data.Foldable as Foldable
import qualified Data.Traversable as Traversable
import Data.Fix
import Data.Monoid
import Data.Maybe

import Control.Arrow
import Control.Monad.State
import Control.Applicative
import Control.Applicative.WrapMonadDual

import Graphics.Gloss(yellow, Point)
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.PictureF
import Graphics.Gloss.Data.PictureF.Trans
import Graphics.Gloss.Data.Ext
import Graphics.Gloss.Data.Ext2
import Graphics.Gloss.Data.Ext.Utils
import Graphics.Gloss.Data.Matrix
import Text.Printf

type PictureS = PictureA SState

data SState = SState { selExt    :: Maybe Ext2
                     , selMatrix :: Maybe Matrix
                     , selInExt  :: Maybe Bool
                     }

instance Show SState where
  show SState{..} = printf "\n(%s,%s,%s)\n"
                    (maybe "-" show selExt)
                    (maybe "-" show selMatrix)
                    (maybe "-" show selInExt)

initSState :: SState
initSState = SState { selExt    = Nothing
                    , selMatrix = Nothing
                    , selInExt  = Nothing
                    }

evalSelectionInfo :: ViewPort -> Point -> Picture -> PictureS
evalSelectionInfo viewPort point pic = pic3
  where
    pic0 :: PictureS
    pic0 = annotateCata (const initSState) $
           eliminateFixedSize viewPort pic

    pic1 :: PictureS
    pic1 = annotateCata extAlg pic0
      where
        extAlg :: (SState, PictureF SState) -> SState
        extAlg (oldState, picture) =
          let newExt = ext2Alg <$> Traversable.traverse selExt picture
          in  oldState { selExt = newExt }

    pic2 :: PictureS
    pic2 = annotateAna matAlg ( (fst $ unFix pic1) { selMatrix = Just mempty }
                              , pic1
                              )
      where
        matAlg :: (SState, (SState, PictureF (        PictureS))) ->
                                    PictureF (SState, PictureS)
        matAlg (currState,(_oldState,picture)) =
          let newMat = do
                currMat <- selMatrix currState
                return $ currMat <> getMatrix picture
              amendMat p = let (st,_p') = unFix p
                               st' = st { selMatrix = newMat }
                           in  (st', p)
          in  fmap amendMat picture

    pic3 :: PictureS
    pic3 = annotateCata inPicAlg pic2
      where
        inPicAlg :: (SState, PictureF SState) -> SState
        inPicAlg (oldState, _pic) =
          let inExt = do
                Ext2{..} <- selExt oldState
                mat <- selMatrix oldState
                let localPoint = applyMatrix (invertMatrix mat) point
                    isIn = pointInExt weakExt localPoint
                return isIn
          in oldState { selInExt = inExt }


annotationUnderPoint :: ViewPort -> Point  -> Picture -> Maybe Annotation
annotationUnderPoint viewPort point pic = topAnnotation
  where
    topAnnotation :: Maybe Annotation
    topAnnotation = cataWithAnnotation getAnnAlg $
                    evalSelectionInfo viewPort point pic
      where
        getAnnAlg :: SState -> PictureF (Maybe Annotation) -> Maybe Annotation
        getAnnAlg currState picture =
          case selInExt currState of
            Just True -> case picture of
              Annotate ann _ -> Just ann
              _ -> getLast $ Foldable.foldMap Last picture
            _ -> Nothing

selectWithExt :: ViewPort -> Point -> Picture -> (Maybe Picture, Picture)
selectWithExt viewPort point = select viewPort point extBorder
  where
    extBorder :: Picture -> Picture
    extBorder pic = let ext2 = getPictureExt2 pic
                    in pictures [ color yellow
                                $ drawExt2 ext2
                                , pic
                                ]

-- TODO: It's actually a lens!
select :: ViewPort -> Point -> (Picture -> Picture) ->
          Picture -> (Maybe Picture, Picture)
select viewPort point selectionTrans pic = pic'
  where
    pic' :: (Maybe Picture, Picture)
    pic' = first getFirst $
           paraWithAnnotation transAlg $
           evalSelectionInfo viewPort point pic
      where
        transAlg :: SState ->
                    PictureF ((First Picture, Picture), PictureS) ->
                              (First Picture, Picture)
        transAlg currState picture =
          let oldPic' = wrap $ deAnn . snd <$> picture
              selPics = Foldable.fold $ fst . fst <$> picture
              picture' = (first snd) <$> picture
          in
          first (<> selPics) $
            case selInExt currState of
              Just True -> if isSelectablePic picture
                           then (First (Just oldPic'),) $
                                selectionTrans $ oldPic'
                           else (mempty,) $
                                 wrap $ switchLastInExt picture'
              _ -> (mempty, oldPic')

        switchLastInExt :: Traversable.Traversable f =>
                           f (Picture, PictureS) ->
                           f (Picture)
        switchLastInExt f = flip evalState False .
                            unwrapMonadDual .
                            Traversable.sequenceA .
                            (WrapMonadDual <$>) .
                            (act <$>) $
                            f
          where
            act :: (Picture, PictureS) -> State Bool (Picture)
            act (newPic, oldPic) = do
              wasMatch <- get
              let oldPic' = deAnn oldPic
              if wasMatch
              then return oldPic'
              else do
                let inExt = fromMaybe False . selInExt . fst $
                            unFix oldPic
                if inExt
                then do
                  put True
                  return newPic
                else
                  return oldPic'

        deAnn :: PictureA a -> Picture
        deAnn = annotateCata (const ())

        isSelectablePic :: PictureF a -> Bool
        isSelectablePic Blank         = True
        isSelectablePic Polygon{}     = True
        isSelectablePic Circle{}      = True
        isSelectablePic Arc{}         = True
        isSelectablePic ThickCircle{} = True
        isSelectablePic ThickArc{}    = True
        isSelectablePic Text{}        = True
        isSelectablePic Bitmap{}      = True
        isSelectablePic Group{}       = True
        isSelectablePic SelectionTrigger{} = True

        isSelectablePic Line{}        = False
        isSelectablePic _             = False
