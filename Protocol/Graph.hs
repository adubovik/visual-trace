{-# language
   RecordWildCards
 , DeriveDataTypeable
 #-}

module Protocol.Graph
 ( Image
 -- Abstract for Server
 , Command(..)
 , mkImage
 , action
 , drawAnn
 , draw
 , evolution
 ) where

import Data.Graph.Dynamic.Annotated
import Data.Graph.Layout

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe(fromMaybe)
import Data.Typeable

import Graphics.Gloss.Interface.Pure.Game(MouseButton(..))
import Graphics.Gloss.Data.EventInfo.Utils
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.ViewPort
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Text as T
import Graphics.Gloss.Data.PictureF
import Graphics.Gloss.Data.PictureF.Trans
import Graphics.Gloss.Data.Feedback

data Command = InsertEdge Key Key
             | InsertNode Key Point
  deriving (Show, Read, Eq, Ord)

type Key = Int
type Graph = Graph2D () () Key

data Image = Image
  { graph2d :: Graph
  , nodeUnderMouse :: Maybe (Node Key)
  , nodeAnnotation :: Maybe (Point, Node Key)
  }
  deriving (Show, Read, Eq, Ord, Typeable)

onGraph :: (Graph -> Graph) -> (Image -> Image)
onGraph f im = im { graph2d = f (graph2d im) }

mkImage :: Image
mkImage = Image { graph2d = empty
                , nodeUnderMouse = Nothing
                , nodeAnnotation = Nothing
                }

action :: Command -> Image -> Image
action (InsertEdge fr to) = onGraph $ insertEdge ((fr,to),Nothing)
action (InsertNode node pos) =
  onGraph $ insertNode (node, Just ((), pos))

drawAnn :: Image -> Picture
drawAnn Image{..} = pictures $
                      edgePics ++
                      nodePics ++
                      [annotationPic nodeAnnotation]
  where
    annotationPic Nothing = blank
    annotationPic (Just (mousePos,node)) =
      color G.black $
      uncurry translate annotPos $
      T.textWithBackground oneLineHeight (G.greyN 0.8) $
      ("Annotation\n" ++ show node)
      where
        oneLineHeight = Just 20
        -- TODO: + (20,20) in terms of real screen coordinates
        annotPos = mousePos + (20,20)

    edgePics :: [Picture]
    edgePics = map drawEdge $ Set.toList edges
      where
        findPos node = fromMaybe err $ Map.lookup node nodePoss
          where
            err = error $ "drawAnn: Can't find position of " ++ show node

        drawEdge (fr,to) = color G.white $
                           line [ findPos fr
                                , findPos to
                                ]

    edges :: Set.Set (Edge Key)
    edges = getEdges graph2d

    nodePoss :: Map.Map (Node Key) Point
    nodePoss = Map.map snd $ getNodeAnnotations graph2d

    nodePics :: [Picture]
    nodePics = map drawNode $ Map.toList nodePoss
      where
        nodeColor node | Just node' <- nodeUnderMouse
                       , node == node'
                       = G.red
                       | otherwise = G.green

        drawNode (node, pos) = color (nodeColor node) $
                               uncurry translate pos $
                               -- annotate ("Annotation\n" ++ show node) $
                               selectionTrigger (ExWrap $ nodeFeedback (node,pos)) $
                               pictures [
                                 translate 0 20.0 (circle 5.0) ,
                                 fixWidth 20 $
                                 pictures $
                                 [ circle 20.0
                                 , circle 10.0
                                 ]
                               ]

        nodeFeedback :: (Node Key, Point) -> Feedback Image
        nodeFeedback (node,_pos) =
          mkFeedback focusCapture feedbackId $
            mkCompFeedback (traceSideEffect feedbackId) transform
          where
            focusCapture = keepFocusedIf (mouseButtonDrag LeftButton)
            feedbackId   = show node

            transform =
              onHoverIn hoverOn `andWhen`
              onHoverOut hoverOff `andWhen`
              onMouseDrag LeftButton moveNode `andWhen`
              onMouseMove mkAnnotation `andWhen`
              onHoverOut  rmAnnotation

            mkAnnotation _old newPos image =
              image { nodeAnnotation = Just (newPos, node) }
            rmAnnotation image = image { nodeAnnotation = Nothing }

            hoverOn  image = image { nodeUnderMouse = Just node }
            hoverOff image = image { nodeUnderMouse = Nothing   }

            moveNode _oldPos newPos =
              -- TODO: replace with "+ (newPos - oldPos)"
              onGraph (adjustNodePos (const newPos) node)

evolution :: Float -> Image -> Image
evolution _secElapsed = onGraph $ fst . applyForces stdForces

-- Common logic for all protocols

draw :: ViewPort -> Image -> G.Picture
draw vp = toPicture vp . drawAnn
