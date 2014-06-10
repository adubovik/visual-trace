{-# language
   RecordWildCards
 , RankNTypes
 , MultiWayIf
 #-}

module VisualTrace.Client.Circles(main) where

import Options.Applicative

import VisualTrace.Protocol.Circles
import VisualTrace.Data.ColorRead(fromColor)
import VisualTrace.Client hiding (send)
import Graphics.Gloss.Data.Color

type Sender = forall a. Show a => a -> IO ()

runCircles :: (Float -> Sender) -> IO ()
runCircles send = do
  send 1.0 $
    InitCircle "Circle1" $
      Circle (100,100) 50.0 (fromColor red)

  send 1.0 $
    InitCircle "Circle2" $
      Circle (0,-50) 100.0 (fromColor green)

  send 1.0 $
    InitCircle "Circle3" $
      Circle (-100,0.0) 150.0 (fromColor blue)

  send 5.0 $
    ChangeRaduis "Circle3" 50.0

main :: IO ()
main = mainWrapper $ do
  config <- execParser httpOptInfo
  let send :: Float -> Sender
      send delay a =
        delaySec delay >>
        sendWithConfig config a

  runCircles send
