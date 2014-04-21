module Debug.Utils where

import Debug.Trace

dbgs :: Show a => String -> a -> a
dbgs s a = trace (s ++ show a) a
