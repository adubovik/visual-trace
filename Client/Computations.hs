{-# LANGUAGE
   OverloadedStrings
 , TupleSections
 #-}

module Client.Computations(main) where

import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as BS
import qualified Codec.Binary.UTF8.String as UTF8
import Text.Printf
import Control.Applicative
import Control.Concurrent
import Control.Monad
import System.Random
import Data.List
import Data.Function

import Protocol.ParallelComputation
import Graphics.Gloss.Data.ColorRead(fromColor)
import Graphics.Gloss.Data.Color

mkCommand :: Int -> Int -> IO [(Int, Command)]
mkCommand nodeId wuid = do
  let nodeId' = printf "Node%03d" nodeId
      wuId' = printf "Wu%03d_%03d" wuid nodeId

      genRndTime = randomRIO (0,1000000)

      mkSubCommand ((statusMsg, clr), time) = do
        (time,) $
          Workunit nodeId' wuId' (fromColor clr, Just statusMsg) (statusMsg ++ " " ++ wuId')

      statuses = [ ("Starting"  , red)
                 , ("Processing", yellow)
                 , ("Finished"  , green)
                 ]

  times <- sort <$> (sequence $ replicate 3 genRndTime)
  let commands = map mkSubCommand $ zip statuses times
  return commands

mkCommands :: Int -> Int -> IO [Command]
mkCommands nodes wusPerNode = do
  commands <- sequence [ mkCommand node wu
                       | node <- [1..nodes]
                       , wu   <- [1..wusPerNode]
                       ]
  let commands' = map snd $ sortBy (compare `on` fst) $ concat commands
  return commands'

sendTree :: IO ()
sendTree = do
  let sendWithDelay msg = do
        threadDelay 10000
        send msg
  commands <- mkCommands 6 10
  mapM_ (sendWithDelay . show) commands

send :: String -> IO ()
send msg = do
  putStrLn $ printf "Sending %s..." msg
  req <- parseUrl "http://localhost:8888"
  let req' = req { method = "POST"
                 , requestBody = RequestBodyLBS $ BS.pack $
                                 UTF8.encode msg
                 }
  void $ withManager $ httpLbs req'

main :: IO ()
main = sendTree

