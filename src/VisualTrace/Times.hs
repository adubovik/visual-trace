{-# language
   RecordWildCards
 , DoAndIfThenElse
 #-}

module VisualTrace.Times
 ( Times
 , mkTimes
 , putTimeStamp
 , getCount
 , updateTimes
 ) where

import Data.Time.Clock.POSIX
import qualified Data.Set as Set

type TimeStamp = Double

data Times = Times
  { tStart :: TimeStamp
  , tCount :: Int
  , tRate  :: Double
  , tStamps :: Set.Set TimeStamp
  }

mkTimes :: Double -> IO Times
mkTimes rate = do
  ts <- getTimeStamp
  return Times { tStart  = ts
               , tCount  = 0
               , tRate   = rate
               , tStamps = Set.empty
               }

getTimeStamp :: IO TimeStamp
getTimeStamp = do
  timeStamp <- getPOSIXTime
  return $ fromRational (toRational timeStamp)

putTimeStamp :: Times -> IO Times
putTimeStamp times@Times{..} = do
  tNow <- getTimeStamp
  let times' = times { tStamps = Set.insert tNow tStamps }
  updateTimes' tNow times'

updateTimes :: Times -> IO Times
updateTimes times = do
  tNow <- getTimeStamp
  updateTimes' tNow times

updateTimes' :: Double -> Times -> IO Times
updateTimes' tNow times@Times{..} = do
  if (tStart <= tNow - tRate)
  then do
    let tCount' = Set.size . snd .
                  Set.split (tNow - tRate) $
                  tStamps
    return $ times { tStart = tNow
                   , tCount = tCount'
                   , tStamps = Set.empty
                   }
  else
    return times

getCount :: Times -> Int
getCount = tCount
