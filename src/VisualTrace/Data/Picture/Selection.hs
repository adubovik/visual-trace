{-# language
   ViewPatterns
 , TupleSections
 , TypeOperators
 , ScopedTypeVariables
 , Rank2Types
 , RecordWildCards
 , DoAndIfThenElse
 #-}

module VisualTrace.Data.Picture.Selection (
   select
 , selectWithExt
 ) where

import qualified Data.Foldable as Foldable
import qualified Data.Traversable as Traversable
import VisualTrace.Data.Fix
import Data.Monoid
import Data.Maybe

import Control.Arrow
import Control.Monad.State
import Control.Applicative
import Control.Applicative.WrapMonadDual

import Graphics.Gloss(yellow, Point)
import VisualTrace.Data.Picture
import VisualTrace.Data.Ext
import VisualTrace.Data.Ext.Utils
import VisualTrace.Data.Matrix
import Text.Printf

type PictureS = PictureA Float SState

data SState = SState { selExt    :: Maybe Ext
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

evalSelectionInfo :: Point -> PictureL -> PictureS
evalSelectionInfo point pic = pic3
  where
    pic0 :: PictureS
    pic0 = annotateCata (const initSState) pic

    pic1 :: PictureS
    pic1 = annotateCata annExtAlg pic0
      where
        annExtAlg :: (SState, PictureF Float SState) -> SState
        annExtAlg (oldState, picture) =
          let newExt :: Maybe Ext = extAlg <$> Traversable.traverse selExt picture
          in  oldState { selExt = newExt }

    pic2 :: PictureS
    pic2 = annotateAna matAlg ( (fst $ unFix pic1) { selMatrix = Just mempty }
                              , pic1
                              )
      where
        matAlg :: (SState, (SState, PictureFL (        PictureS))) ->
                                    PictureFL (SState, PictureS)
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
        inPicAlg :: (SState, PictureFL SState) -> SState
        inPicAlg (oldState, _pic) =
          let inExt = do
                ext <- selExt oldState
                mat <- selMatrix oldState
                let localPoint = applyMatrix (invertMatrix mat) point
                    isIn = pointInExt ext localPoint
                return isIn
          in oldState { selInExt = inExt }

selectWithExt :: Point -> PictureL -> (Maybe PictureL, PictureL)
selectWithExt point = select point extBorder
  where
    extBorder :: PictureL -> PictureL
    extBorder pic = let ext = getPictureExt pic
                    in pictures [ color yellow
                                $ drawExt NoFill ext
                                , pic
                                ]

-- TODO: It's actually a lens!
select :: Point -> (PictureL -> PictureL) ->
          PictureL -> (Maybe PictureL, PictureL)
select point selectionTrans pic = pic'
  where
    pic' :: (Maybe PictureL, PictureL)
    pic' = first getFirst $
           paraWithAnnotation transAlg $
           evalSelectionInfo point pic
      where
        transAlg :: SState ->
                    PictureFL ((First PictureL, PictureL), PictureS) ->
                               (First PictureL, PictureL)
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
                           f (PictureL, PictureS) ->
                           f (PictureL)
        switchLastInExt f = flip evalState False .
                            unwrapMonadDual .
                            Traversable.sequenceA .
                            (WrapMonadDual <$>) .
                            (act <$>) $
                            f
          where
            act :: (PictureL, PictureS) -> State Bool (PictureL)
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

        deAnn :: PictureA Float a -> PictureL
        deAnn = annotateCata (const ())

        isSelectablePic :: PictureF d a -> Bool
        isSelectablePic Blank  = True
        isSelectablePic Arc{}  = True
        isSelectablePic Text{} = True
        isSelectablePic SelectionTrigger{} = True

        isSelectablePic Line{} = False
        isSelectablePic _      = False
