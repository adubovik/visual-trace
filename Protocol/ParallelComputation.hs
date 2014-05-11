{-# language
   RecordWildCards
 , DeriveDataTypeable
 #-}

module Protocol.ParallelComputation
 ( Image
 -- Abstract for Server
 , Command(..)
 , mkImage
 , action
 , drawAnn
 , draw
 , evolution
 )
 where

import Data.Monoid
import Data.Maybe
import Data.Typeable
import qualified Data.Map as Map
import Text.Printf

import qualified Graphics.Gloss.Text as T
import Graphics.Gloss.Data.ViewPort
import qualified Graphics.Gloss as G
import Graphics.Gloss.Data.EventInfo.Utils
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.PictureF
import Graphics.Gloss.Data.PictureF.Trans
import Graphics.Gloss.Data.ColorRead(Color,fromColor,toColor)

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

data HighlightedWorkunit = HWu
  { hwuMousePos :: Point
  , hwuWuId :: WorkunitId
  , hwuWu :: Workunit
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
  , highlightedWorkunit :: Maybe HighlightedWorkunit
  }
  deriving (Show, Read, Eq, Ord, Typeable)

onNodeMap :: (Map.Map NodeId Workunits -> Map.Map NodeId Workunits) ->
             (Image -> Image)
onNodeMap f im = im { nodeMap = f (nodeMap im) }

mkImage :: Image
mkImage = Image { nodeMap = Map.empty
                , highlightedWorkunit = Nothing
                }

action :: Command -> Image -> Image
action Workunit{..} = onNodeMap modifyNodeMap
  where
    workunit = Wu { wuStatus = wuSt
                  , wuHistory = [(wuSt, wuMsg)]
                  }

    modifyNodeMap = Map.insertWith (Map.unionWith (<>)) wuNodeId
                      (Map.singleton wuId workunit)

drawAnn :: Image -> Picture
drawAnn Image{..} =
  pictures
    [ rvcat nodesPadding $ map (uncurry drawNode) $ Map.toList nodeMap
    , maybe blank drawHighlightedWorkunit highlightedWorkunit
    ]
  where
    nodesPadding           = 10
    nodeRectPadding        = 10
    nodeHeader_BodyPadding = 10
    nodeIdRectPadding      = 5
    tableWHRatio           = 2.0
    nodeIdTextHeight       = 50
    wuStatusTextHeight     = 50
    wuStatusRectPadding    = 3
    tableVPadding          = 10
    tableHPadding          = 10
    annotationFontHeight   = 20

    preprocessStatus :: String -> String
    preprocessStatus s =
      let n = 3
          s' = take n s
          n' = length s'
          suffix = replicate (n-n') ' '
      in s' ++ suffix

    drawHighlightedWorkunit :: HighlightedWorkunit -> Picture
    drawHighlightedWorkunit HWu{..} =
        color G.black $
        uncurry translate annotPos $
        T.textsWithBackground oneLineHeight textRows
      where
        textRows = (G.greyN 0.8, hwuWuId) : map formRow wuHistory
        formRow ((clr,status), msg) = ( toColor clr
                                      , fromMaybe "-" status ++ ": " ++ msg
                                      )
        oneLineHeight = Just annotationFontHeight
        -- TODO: + (20,20) in terms of real screen coordinates
        annotPos = hwuMousePos + (20,20)
        Wu{..} = hwuWu

    drawNode :: NodeId -> Workunits -> Picture
    drawNode nodeId workunits =
      insideRect Fill nodeRectPadding (Just $ G.greyN 0.5) $
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

        nodeHeader = insideRect Fill nodeIdRectPadding (Just G.blue) $
                     drawText nodeIdTextHeight nodeId

    drawText :: Float -> String -> Picture
    drawText targetHeight s =
      let factor = targetHeight/(T.textHeight s)
      in color G.black $
         scale factor factor $ T.text s

    drawTable :: Int -> Int -> [Picture] -> Picture
    drawTable w _h cells =
      rvcat tableVPadding $
      map (hcat tableHPadding) cells'
      where
        cells' = splitAtChunks w cells

        splitAtChunks _ [] = []
        splitAtChunks chunkSize ls =
          let (hd,tl) = splitAt chunkSize ls
          in  hd : splitAtChunks chunkSize tl

    drawWorkunit :: NodeId -> WorkunitId -> Workunit -> Picture
    drawWorkunit nodeId workunitId workunit@Wu{..} =
      selectionTrigger (ExWrap $ wuFeedback nodeId workunitId workunit) $
      insideRect Fill wuStatusRectPadding (Just $ toColor clr) $
      drawText wuStatusTextHeight $
      preprocessStatus $
      fromMaybe "" status
      where
        (clr, status) = wuStatus

    wuFeedback :: NodeId -> WorkunitId -> Workunit -> Feedback Image
    wuFeedback nodeId wuId wu = Feedback
      { fbSideEffect = sideEffect
      , fbTransform  = transform
      , fbId         = show (nodeId, wuId)
      , fbFocusCapture = focusCapture
      }
      where
        focusCapture = stdFocusCapture

        sideEffect event _image = do
         putStrLn $ printf "Event %s on %s " (show event) (show (nodeId, wuId))

        transform = onMouseMove focusCapture highlightWu `andWhen`
                    onHoverOut focusCapture hoverOff

        highlightWu _oldPos newPos image =
          image { highlightedWorkunit = Just $ HWu
                    { hwuMousePos = newPos
                    , hwuWuId = wuId
                    , hwuWu = wu
                    }
                }

        hoverOff image = image { highlightedWorkunit = Nothing }

evolution :: Float -> Image -> Image
evolution _secElapsed = id

-- Common logic for all protocols

draw :: ViewPort -> Image -> G.Picture
draw vp = toPicture vp . drawAnn

