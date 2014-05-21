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

import qualified Graphics.Gloss as G
import Graphics.Gloss.Data.Point

import qualified VisualTrace.Text as T
import VisualTrace.Data.EventInfo.Utils
import VisualTrace.Data.EventInfo.StdLib
import VisualTrace.Data.PictureF
import VisualTrace.Data.ColorRead(Color,fromColor,toColor)
import VisualTrace.Data.Feedback
import qualified VisualTrace.Protocol.Image as I

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

data Workunit = Wu
  { wuStatus :: WorkunitStatus
  -- Including the current status
  , wuHistory :: [(WorkunitStatus, WorkunitMessage)]
  }
  deriving (Show, Read, Eq, Ord, Typeable)

data AnnotatedWorkunit = AWu
  { awuMousePos :: Point
  , awuWuId :: WorkunitId
  , awuWu :: Workunit
  }
  deriving (Show, Read, Eq, Ord, Typeable)

instance Monoid Workunit where
  mempty = Wu { wuStatus = (fromColor G.white, Nothing)
              , wuHistory = []
              }
  a `mappend` b = Wu { wuStatus = wuStatus a
                     , wuHistory = wuHistory b ++ wuHistory a
                     }

data Command = Workunit
  { wuNodeId :: NodeId
  , wuId     :: WorkunitId
  , wuSt     :: WorkunitStatus
  , wuMsg    :: WorkunitMessage
  }
   deriving (Show, Read, Eq, Ord)

data Image = Image
  { nodeMap :: Map.Map NodeId Workunits
  , annotatedWorkunit :: Maybe AnnotatedWorkunit
  }
  deriving (Show, Read, Eq, Ord, Typeable)

instance I.Image Image where
  type Command Image = Command
  initImage = mkImage
  drawImageG = drawAnn'
  evolveImage = evolution
  interpret = action

onNodeMap :: (Map.Map NodeId Workunits -> Map.Map NodeId Workunits) ->
             (Image -> Image)
onNodeMap f im = im { nodeMap = f (nodeMap im) }

mkImage :: Image
mkImage = Image { nodeMap = Map.empty
                , annotatedWorkunit = Nothing
                }

action :: Command -> Image -> Image
action Workunit{..} = onNodeMap modifyNodeMap
  where
    workunit = Wu { wuStatus = wuSt
                  , wuHistory = [(wuSt, wuMsg)]
                  }

    modifyNodeMap = Map.insertWith (Map.unionWith (<>)) wuNodeId
                      (Map.singleton wuId workunit)

drawAnn' :: Image -> PictureG
drawAnn' Image{..} =
  pictures
    [ rvcat nodesPadding $ map (uncurry drawNode) $ Map.toList nodeMap
    , maybe blank drawAnnotatedWorkunit annotatedWorkunit
    ]
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
    annotationFontHeight   = 20

    preprocessStatus :: String -> String
    preprocessStatus s =
      let n = 3
          s' = take n s
          n' = length s'
          suffix = replicate (n-n') ' '
      in s' ++ suffix

    drawAnnotatedWorkunit :: AnnotatedWorkunit -> PictureG
    drawAnnotatedWorkunit AWu{..} =
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
        Wu{..} = awuWu

    drawNode :: NodeId -> Workunits -> PictureG
    drawNode nodeId workunits =
      insideRect nodeRectPadding Fill (Just $ G.greyN 0.5) $
        rvcat nodeHeader_BodyPadding $ [ nodeHeader
                                       , drawTable width height workunits'
                                       ]
      where
        workunits' = map (uncurry $ drawWorkunit nodeId) $ Map.toList workunits
        (width, height) = findWH tableWHRatio (Map.size workunits)

        findWH :: Float -> Int -> (Int,Int)
        findWH ratio size =
          let hf = sqrt (fromIntegral size/ratio)
              h = max 1 (round hf)
              w = (size + h - 1) `div` h
          in  (w,h)

        nodeHeader = insideRect nodeIdRectPadding Fill (Just G.blue) $
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
    drawWorkunit nodeId workunitId workunit@Wu{..} =
      selectionTrigger (wuFeedback nodeId workunitId workunit) $
      insideRect wuStatusRectPadding Fill (Just $ toColor clr) $
      drawText wuStatusTextHeight $
      preprocessStatus $
      fromMaybe "" status
      where
        (clr, status) = wuStatus

    wuFeedback :: NodeId -> WorkunitId -> Workunit -> Feedback Image
    wuFeedback nodeId wuId wu =
      mkFeedback stdFocusCapture feedbackId $
        mkCompFeedback (traceSideEffect feedbackId) transform
      where
        feedbackId = show (nodeId, wuId)

        transform = stdAnnotationTransform mkAnnotation rmAnnotation

        mkAnnotation _oldPos newPos image =
          image { annotatedWorkunit = Just $ AWu
                    { awuMousePos = newPos
                    , awuWuId = wuId
                    , awuWu = wu
                    }
                }

        rmAnnotation image = image { annotatedWorkunit = Nothing }

evolution :: Float -> Image -> Image
evolution _secElapsed = id
