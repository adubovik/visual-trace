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
 , getAnnotation
 , evolution
 )
 where

import Data.Monoid
import Data.Maybe
import Data.Typeable
import qualified Data.Map as Map

import qualified Graphics.Gloss.Text as T
import Graphics.Gloss.Data.ViewPort
import qualified Graphics.Gloss as G
import Graphics.Gloss.Data.PictureF
import Graphics.Gloss.Data.PictureF.Selection
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
  }
  deriving (Show, Read, Eq, Ord, Typeable)

onNodeMap :: (Map.Map NodeId Workunits -> Map.Map NodeId Workunits) ->
             (Image -> Image)
onNodeMap f im = im { nodeMap = f (nodeMap im) }

mkImage :: Image
mkImage = Image { nodeMap = Map.empty
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
  rvcat nodesPadding $ map (uncurry drawNode) $ Map.toList nodeMap
  where
    nodesPadding = 10
    nodeRectPadding = 10
    nodeHeader_BodyPadding = 10
    nodeIdRectPadding = 5
    tableWHRatio = 2.0
    nodeIdTextHeight = 50
    wuStatusTextHeight = 50
    wuStatusRectPadding = 3
    tableVPadding = 10
    tableHPadding = 10

    drawNode nodeId workunits =
      insideRect Fill nodeRectPadding (Just $ G.greyN 0.5) $
        rvcat nodeHeader_BodyPadding $ [ nodeHeader
                                       , drawTable width height workunits'
                                       ]
      where
        workunits' = map (uncurry drawWorkunit) $ Map.toList workunits
        (width, height) = findWH tableWHRatio (Map.size workunits)

        findWH :: Float -> Int -> (Int,Int)
        findWH ratio size =
          let hf = sqrt (fromIntegral size/ratio)
              h = max 1 (round hf)
              w = (size + h - 1) `div` h
          in  (w,h)

        nodeHeader = insideRect Fill nodeIdRectPadding (Just G.blue) $
                     text' nodeIdTextHeight nodeId

        text' targetHeight s =
          let factor = targetHeight/(T.textHeight s)
          in color G.black $
             scale factor factor $ T.text s

        splitAtChunks _ [] = []
        splitAtChunks chunkSize ls =
          let (hd,tl) = splitAt chunkSize ls
          in  hd : splitAtChunks chunkSize tl

        drawTable w _h cells =
          let cells' = splitAtChunks w cells
          in  rvcat tableVPadding $
              map (hcat tableHPadding) cells'

        drawWorkunit workunitId Wu{..} =
          let (clr, status) = wuStatus
          in  annotate (unlines $ (workunitId : map show wuHistory)) $
              insideRect Fill wuStatusRectPadding (Just $ toColor clr) $
              text' wuStatusTextHeight $
              fromMaybe " " status

evolution :: Float -> Image -> Image
evolution _secElapsed = id

-- Common logic for all protocols

getAnnotation :: ViewPort -> (Float, Float) -> Image -> Maybe String
getAnnotation viewPort mousePos = annotationUnderPoint viewPort mousePos . drawAnn

draw :: ViewPort -> Image -> G.Picture
draw vp = toPicture vp . drawAnn

