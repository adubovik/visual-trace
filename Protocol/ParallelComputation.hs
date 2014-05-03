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
import Data.Typeable
import qualified Data.Map as Map

import Graphics.Gloss.Data.ViewPort
import qualified Graphics.Gloss as G
import Graphics.Gloss.Data.PictureF
import Graphics.Gloss.Data.PictureF.Selection
import Graphics.Gloss.Data.PictureF.Trans
import Graphics.Gloss.Data.ColorRead(Color,toColor,fromColor)

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
  a `mappend` b = Wu { wuStatus = wuStatus b
                     , wuMsgLog = wuMsgLog a ++ wuMsgLog b
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
  vcat 10 $ map (const $
    hcat 10 $ map (const $
      color G.red $ circle 10)
    [(0::Int)..10])
  [(0::Int)..20]

evolution :: Float -> Image -> Image
evolution _secElapsed = id

-- Common logic for all protocols

getAnnotation :: ViewPort -> (Float, Float) -> Image -> Maybe String
getAnnotation viewPort mousePos = annotationUnderPoint viewPort mousePos . drawAnn

draw :: ViewPort -> Image -> G.Picture
draw vp = toPicture vp . drawAnn

