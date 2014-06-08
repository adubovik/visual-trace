{-# language
   RecordWildCards
 #-}

module VisualTrace.Data.Interval where

import Text.Printf

data R v = MinusInf | R !v | PlusInf deriving (Eq, Ord)

instance Show v => Show (R v) where
  show MinusInf = "-Inf"
  show PlusInf  = "+Inf"
  show (R v) = show v

instance Bounded (R v) where
  minBound = MinusInf
  maxBound = PlusInf

data Boundary = Open | Closed deriving (Eq, Ord)

data Interval v = Interval { ltype :: !Boundary
                           , low   :: !(R v)
                           , high  :: !(R v)
                           , htype :: !Boundary
                           } deriving (Eq, Ord)

instance Show v => Show (Interval v) where
  show (Interval o l h c) =
    printf "%s%s,%s%s" (opening o) (show l) (show h) (closing c)
    where
      opening Open   = "("
      opening Closed = "["
      closing Open   = ")"
      closing Closed = "]"

closedInterval :: Ord a => (a,a) -> Interval a
closedInterval (x,y) = Interval Closed (R x) (R y) Closed

subinterval :: Ord v => Interval v -> Interval v -> Bool
subinterval smaller bigger =
  low bigger <= low smaller && high smaller <= high bigger

intersects :: Ord v => Interval v -> Interval v -> Bool
intersects one two = low one `inside` two || high one `inside` two ||
                     low two `inside` one || high two `inside` one

inside :: Ord v => R v -> Interval v -> Bool
inside p Interval{..} =
  (cmp ltype) low p && (cmp htype) p high
    where
      cmp Open   = (<)
      cmp Closed = (<=)

merge :: Ord v => Interval v -> Interval v -> Interval v
merge i1 i2 | i1 <= i2  = Interval (ltype i1) (low i1) (high i2) (htype i2)
            | otherwise = merge i2 i1
