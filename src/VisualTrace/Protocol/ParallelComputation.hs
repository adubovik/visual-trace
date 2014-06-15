{-# language
   RecordWildCards
 , DeriveDataTypeable
 , TypeFamilies
 #-}

module VisualTrace.Protocol.ParallelComputation
 ( Image
 -- Abstract for Server
 , Command(..)
 )
 where

import Data.Monoid
import Data.Maybe
import Data.Typeable
import qualified Data.Map as Map
import qualified Data.Set as Set

import Graphics.Gloss.Interface.Pure.Game(MouseButton(..))
import qualified Graphics.Gloss as G
import Graphics.Gloss.Data.Point

import qualified VisualTrace.Text as T
import VisualTrace.Data.EventInfo.Utils
import VisualTrace.Data.EventInfo.StdLib
import VisualTrace.Data.Picture
import VisualTrace.Data.ColorRead(Color,fromColor,toColor)
import VisualTrace.Data.Feedback
import qualified VisualTrace.Protocol.Image as I
import VisualTrace.Protocol.Image(ImageGroup,AuxImage,onAuxImage,onBaseImage)

-- Identifier of computation node
-- that processing a workunit.
type NodeId = String

type WorkunitId = String
type Workunits = Map.Map WorkunitId Workunit

-- Status could denote workunit states like
-- NotYetStarted, WorkingOn, Finished etc.
-- We give to user unlimited control on possible
-- statuses to make it more flexible.
--
-- Status should be as short as possible (2-3 symbols), because
-- it would be printed inside workunit cells thus it would take
-- place on the screen.
type WorkunitStatus = (Color, Maybe String)
type WorkunitMessage = String

data Workunit = Workunit
  { wuStatus :: WorkunitStatus
  -- Including the current status
  , wuHistory :: [(WorkunitStatus, WorkunitMessage)]
  }
  deriving (Show, Read, Eq, Ord, Typeable)

data AnnotatedWorkunit = AnnotatedWorkunit
  { awuMousePos :: Point
  , awuWuId :: WorkunitId
  , awuWu :: Workunit
  }
  deriving (Show, Read, Eq, Ord, Typeable)

instance Monoid Workunit where
  mempty = Workunit { wuStatus = (fromColor G.white, Nothing)
                    , wuHistory = []
                    }
  a `mappend` b = Workunit { wuStatus = wuStatus a
                           , wuHistory = wuHistory b ++ wuHistory a
                           }

data Command = WorkunitStatus
  { wuNodeId :: NodeId
  , wuId     :: WorkunitId
  , wuSt     :: WorkunitStatus
  , wuMsg    :: WorkunitMessage
  }
  deriving (Show, Read, Eq, Ord)

data Image = Image
  { nodeMap :: Map.Map NodeId Workunits
  , foldedNodes :: Set.Set NodeId
  }
  deriving (Show, Read, Eq, Ord, Typeable)

instance I.Image Image where
  type Command Image = Command
  newtype AuxImage Image = AuxImage
    { annotatedWorkunit :: Maybe AnnotatedWorkunit }

  initBase = (flip const) initBase (mkBigImage 1000 6)
  initAux  = initAux

  drawBaseRaw = drawBaseRaw
  drawAuxRaw = drawAuxRaw

  evolveBase = evolveBase
  evolveAux = evolveAux

  interpretBase = interpretBase
  interpretAux  = const id

initAux :: AuxImage Image
initAux = AuxImage { annotatedWorkunit = Nothing }

initBase :: Image
initBase = Image { nodeMap = Map.empty, foldedNodes = Set.empty }

-- For performance testing
mkBigImage :: Int -> Int -> Image
mkBigImage wusPerNode nNodes =
  initBase { nodeMap = mkNodeMap }
  where
    mkNodeMap = Map.fromList [ (show nId, mkWorkUnits wusPerNode nId)
                             | nId <- [(0::Int)..nNodes-1]
                             ]

    mkWorkUnits :: Int -> Int -> Workunits
    mkWorkUnits n nIdx = Map.fromList
      [ (wuName, Workunit wuStat [(wuStat,wuName)])
      | i <- [0..n-1]
      , let wuName = show nIdx ++ "_" ++ show i
      , let wuStat = (fromColor (G.light G.green), Just wuName)
      ]

onNodeMap :: (Map.Map NodeId Workunits -> Map.Map NodeId Workunits) ->
             (Image -> Image)
onNodeMap f im = im { nodeMap = f (nodeMap im) }

interpretBase :: Command -> Image -> Image
interpretBase WorkunitStatus{..} = onNodeMap modifyNodeMap
  where
    workunit = Workunit { wuStatus = wuSt
                        , wuHistory = [(wuSt, wuMsg)]
                        }

    modifyNodeMap = Map.insertWith (Map.unionWith (<>)) wuNodeId
                      (Map.singleton wuId workunit)

drawAuxRaw :: AuxImage Image -> PictureG
drawAuxRaw AuxImage{..} =
  maybe blank drawAnnotatedWorkunit annotatedWorkunit
  where
    annotationFontHeight   = 20

    drawAnnotatedWorkunit :: AnnotatedWorkunit -> PictureG
    drawAnnotatedWorkunit AnnotatedWorkunit{..} =
        color G.black $
        translateAnnot $
        T.textsWithBackground (Just annotationFontHeight) textRows
      where
        textRows = (G.greyN 0.8, awuWuId) : map formRow wuHistory
        formRow ((clr,status), msg) = ( toColor clr
                                      , fromMaybe "-" status ++ ": " ++ msg
                                      )
        translateAnnot =
          let (x,y) = awuMousePos
          in translate (screen 5) (screen 5) .
             translate (local  x) (local  y)
        Workunit{..} = awuWu

drawBaseRaw :: Image -> PictureG
drawBaseRaw baseImage =
  rvcat nodesPadding $
  map (uncurry drawNode) $
  Map.toList (nodeMap baseImage)
  where
    nodesPadding           = local 10
    nodeRectPadding        = local 10
    nodeHeader_BodyPadding = local 10
    nodeIdRectPadding      = local 5
    tableWHRatio           = 2.0
    nodeIdTextHeight       = 50
    wuStatusTextHeight     = 50
    wuStatusRectPadding    = local 3
    tableVPadding          = local 10
    tableHPadding          = local 10

    preprocessStatus :: String -> String
    preprocessStatus s =
      let n = 3
          s' = take n s
          n' = length s'
          suffix = replicate (n-n') ' '
      in s' ++ suffix

    drawNode :: NodeId -> Workunits -> PictureG
    drawNode nodeId  workunits =
      insideRect nodeRectPadding Fill (Just $ G.greyN 0.5) $
        rvcat nodeHeader_BodyPadding $ [ header
                                       , table
                                       ]
      where
        isFolded = nodeId `Set.member` (foldedNodes baseImage)

        table = if isFolded
                then blank
                else drawTable width height workunits'

        workunits' = map (uncurry $ drawWorkunit nodeId) $ Map.toList workunits
        (width, height) = findWH tableWHRatio (Map.size workunits)

        findWH :: Float -> Int -> (Int,Int)
        findWH ratio size =
          let hf = sqrt (fromIntegral size/ratio)
              h = max 1 (round hf)
              w = (size + h - 1) `div` h
          in  (w,h)

        headerColor | isFolded  = G.dark  G.blue
                    | otherwise = G.light G.blue

        header = insideRect nodeIdRectPadding Fill (Just headerColor) $
                 selectionTrigger (nodeFeedback nodeId) $
                 drawText nodeIdTextHeight nodeId

    drawText :: Float -> String -> PictureG
    drawText targetHeight s =
      let factor = targetHeight / T.textHeight s
      in color G.black $
         scale' (local factor) $ T.text s

    drawTable :: Int -> Int -> [PictureG] -> PictureG
    drawTable w _h cells =
      rvcat tableVPadding $
      map (hcat tableHPadding) cells'
      where
        cells' = splitAtChunks w cells

        splitAtChunks _ [] = []
        splitAtChunks chunkSize ls =
          let (hd,tl) = splitAt chunkSize ls
          in  hd : splitAtChunks chunkSize tl

    drawWorkunit :: NodeId -> WorkunitId -> Workunit -> PictureG
    drawWorkunit nodeId workunitId workunit@Workunit{..} =
      selectionTrigger (wuFeedback nodeId workunitId workunit) $
      insideRect wuStatusRectPadding Fill (Just $ toColor clr) $
      drawText wuStatusTextHeight $
      preprocessStatus $
      fromMaybe "" status
      where
        (clr, status) = wuStatus

    nodeFeedback :: NodeId -> Feedback (ImageGroup Image)
    nodeFeedback nodeId =
      mkFeedback stdFocusCapture feedbackId $
        mkCompFeedback (traceSideEffect feedbackId) transform
      where
        feedbackId = nodeId

        transform = onMouseClick LeftButton foldUnfoldNode

        foldUnfoldNode _currPos = onBaseImage $ \image ->
          let switchElem el set | el `Set.member` set = el `Set.delete` set
                                | otherwise           = el `Set.insert` set
          in  image { foldedNodes = switchElem nodeId (foldedNodes image) }

    wuFeedback :: NodeId -> WorkunitId -> Workunit ->
                  Feedback (ImageGroup Image)
    wuFeedback nodeId wuId wu =
      mkFeedback stdFocusCapture feedbackId $
        mkCompFeedback (traceSideEffect feedbackId) transform
      where
        feedbackId = show (nodeId, wuId)

        transform = stdAnnotationTransform mkAnnotation rmAnnotation

        mkAnnotation _oldPos newPos = onAuxImage $ \image ->
          image { annotatedWorkunit = Just $ AnnotatedWorkunit
                    { awuMousePos = newPos
                    , awuWuId = wuId
                    , awuWu = wu
                    }
                }

        rmAnnotation = onAuxImage $ \image ->
          image { annotatedWorkunit = Nothing }

evolveBase :: Float -> Image -> Image
evolveBase _secElapsed = id

evolveAux :: Float -> AuxImage Image -> AuxImage Image
evolveAux  _secElapsed = id
