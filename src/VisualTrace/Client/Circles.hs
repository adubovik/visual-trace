{-# language
   RecordWildCards
 , RankNTypes
 , MultiWayIf
 #-}

module VisualTrace.Client.Circles(main) where

import Control.Monad
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

  let oscillation :: Float -> Float -> Int -> [Float]
      oscillation from to n =
        [ from + (fromIntegral i)*step
        | i <- [0..n] ++ [n-1,n-2..1]
        , let step = (to - from)/(fromIntegral n)
        ]

      circles :: String -> Float -> Float -> Int -> [Command]
      circles ident from to n = cycle $
        (flip map) (oscillation from to n) $ \rad ->
          ChangeRaduis ident rad

  let go (c1,c2,c3) = do
        let (h1,t1) = splitAt 10 c1
            (h2,t2) = splitAt 10 c2
            (h3,t3) = splitAt 10 c3
            heads = zipWith3 (,,) h1 h2 h3

        forM_ heads $ \(cr1,cr2,cr3) -> do
          send 0.05 cr1
          send 0.05 cr2
          send 0.05 cr3

        go (t1,t2,t3)

  let c1 = circles "Circle1"  50.0 100.0 40
      c2 = circles "Circle2" 100.0 120.0 60
      c3 = circles "Circle3" 150.0 200.0 20

  go (c1,c2,c3)


main :: IO ()
main = mainWrapper $ do
  config <- execParser httpOptInfo
  let send :: Float -> Sender
      send delay a =
        delaySec delay >>
        sendWithConfig config a

  runCircles send
