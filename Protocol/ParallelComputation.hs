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
  , wuMsgLog :: [WorkunitMessage]
  }
  deriving (Show, Read, Eq, Ord, Typeable)

instance Monoid Workunit where
  mempty = Wu { wuStatus = (fromColor G.white, Nothing)
              , wuMsgLog = []
              }
  a `mappend` b = Wu { wuStatus = wuStatus a
                     , wuMsgLog = wuMsgLog b ++ wuMsgLog a
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
                  , wuMsgLog = [wuMsg]
                  }

    modifyNodeMap = Map.insertWith (Map.unionWith (<>)) wuNodeId
                      (Map.singleton wuId workunit)

drawAnn :: Image -> Picture
drawAnn Image{..} =
  rvcat 10 $ map (uncurry drawNode) $ Map.toList nodeMap
  where
    drawNode nodeId workunits =
      insideRect 10 (Just G.white) $
        rvcat 10 $ [ nodeHeader
                   , drawTable width height workunits'
                   ]
      where
        (width, height) = let ratio = 2.0
                          in  findWH ratio (Map.size workunits)

        findWH :: Float -> Int -> (Int,Int)
        findWH ratio size =
          let hf = sqrt (fromIntegral size/ratio)
              h = max 1 (round hf)
              w = (size + h - 1) `div` h
          in  (w,h)

        workunits' = map (uncurry drawWorkunit) $ Map.toList workunits

        nodeHeader = insideRect 5 (Just G.red) $
                     color G.white $ text nodeId

        splitAtChunks _ [] = []
        splitAtChunks chunkSize ls =
          let (hd,tl) = splitAt chunkSize ls
          in  hd : splitAtChunks chunkSize tl

        drawTable w _h cells =
          let cells' = splitAtChunks w cells
          in  rvcat 10 $ map (hcat 10) cells'

        drawWorkunit workunitId Wu{..} =
          let (clr, status) = wuStatus
          in  annotate (unlines (workunitId:wuMsgLog)) $
              insideRect 3 (Just $ toColor clr) $
              color G.white $ text $
              fromMaybe " " status

evolution :: Float -> Image -> Image
evolution _secElapsed = id

-- Common logic for all protocols

getAnnotation :: ViewPort -> (Float, Float) -> Image -> Maybe String
getAnnotation viewPort mousePos = annotationUnderPoint viewPort mousePos . drawAnn

draw :: ViewPort -> Image -> G.Picture
draw vp = toPicture vp . drawAnn

