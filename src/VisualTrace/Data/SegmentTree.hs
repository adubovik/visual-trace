{-# LANGUAGE
   FlexibleInstances
 , FlexibleContexts
 , RecordWildCards
 , TypeFamilies
 , NamedFieldPuns
 , StandaloneDeriving
 , TupleSections
 #-}

module VisualTrace.Data.SegmentTree (
   SegmentTree2D
 , SegmentTree1D

 , mkSegmentTree2D
 , mkSegmentTree1D
 , querySegmentTree2D
 , querySegmentTree1D
 ) where

import Data.List (sort, unfoldr, foldl')
import Data.Monoid
import Text.Printf
import Data.Set(Set)
import qualified Data.Set as Set

import VisualTrace.Data.Interval

data Bin r = Tip | Bin r r
  deriving Show

class SegmentTree a where
  data STree a :: *
  data Intv a :: *
  type Point a :: *
  type MonoidTag a :: *
  fromList :: [Intv a] -> STree a
  query :: STree a -> Point a -> MonoidTag a

mkSkeleton :: Ord a => [Intv (a,b)] -> STree (a,b)
mkSkeleton intervals = skeleton
  where
    [skeleton]:_ = dropWhile (not . converged) $
                   iterate (unfoldr connect) $
                   map simpleLeaf atomicIntervals

    simpleLeaf i = Tree { stdList  = Set.empty
                        , subTree  = error "simpleLeft.empty subTree"
                        , interval = i
                        , branch   = Tip
                        }

    simpleBranch x y = Tree { stdList  = Set.empty
                            , subTree  = error "simpleBranch.empty subTree"
                            , interval = interval x `merge` interval y
                            , branch   = Bin x y
                            }

    connect []         = Nothing
    connect [x,y,z]    = Just $ ((x `simpleBranch` y) `simpleBranch` z, [])
    connect (x:y:rest) = Just $ (x `simpleBranch` y, rest)

    converged [x] = True
    converged _   = False

    atomicIntervals = concatMap toAtomicInterval $
                      zip endpoints (drop 1 endpoints)

    toAtomicInterval (a, b) =
      [Interval Open a b Open] ++
      case b of
        PlusInf -> []
        _       -> [Interval Closed b b Closed]

    endpoints = sort $ [MinusInf, PlusInf] ++
                       concatMap (\i -> [low i,high i]) baseIntervals
    baseIntervals = map (\(Intv i _) -> i) intervals

subInsert :: (Ord a, Ord (Intv b)) => Intv (a,b) -> STree (a,b) -> STree (a,b)
subInsert itv@(Intv i is) t@Tree{interval=baseInterval, branch, stdList} =
  case branch of
    Tip
      | baseInterval `subinterval` i
      -> t { stdList = Set.insert itv stdList }
      | otherwise
      -> t
    Bin left right
      | baseInterval `subinterval` i
      -> t { stdList = Set.insert itv stdList }
      | otherwise
      ->
        let left'  = if i `intersects` (interval left)
                     then subInsert itv left
                     else left
            right' = if i `intersects` (interval right)
                     then subInsert itv right
                     else right
        in  t { branch = Bin left' right' }

finalize :: (Ord (Intv b), SegmentTree b) => STree (a,b) -> STree (a,b)
finalize t@Tree{..} =
  let subTree' = fromList $
                 map (\(Intv _ b) -> b) $
                 Set.toList stdList
      t' = t { subTree = subTree' }
  in  case branch of
    Tip     -> t'
    Bin l r -> t' { branch = Bin (finalize l) (finalize r) }

deriving instance (Eq   a, Eq   (Intv  b)) => Eq   (Intv  (a,b))
deriving instance (Ord  a, Ord  (Intv  b)) => Ord  (Intv  (a,b))
deriving instance (Show a, Show (Intv  b)) => Show (Intv  (a,b))
deriving instance (Show a, Show (Intv  b)
                         , Show (STree b)) => Show (STree (a,b))

instance (Monoid (MonoidTag b), Ord a, Ord (Intv b), SegmentTree b) =>
         SegmentTree (a,b) where
  data Intv      (a,b) = Intv (Interval a) (Intv b)
  type Point     (a,b) = (a,Point b)
  type MonoidTag (a,b) = MonoidTag b
  data STree     (a,b) = Tree
    { stdList  :: !(Set (Intv (a,b)))
    , subTree  ::  (STree b)
    , interval :: !(Interval a)
    , branch   :: !(Bin (STree (a,b)))
    }

  fromList intervals =
    finalize $ foldl' (flip subInsert) (mkSkeleton intervals) intervals

  query t@Tree{interval=baseInterval,subTree,branch} ps@(point,ps') =
    let continue = (R point) `inside` baseInterval
        tag | continue = query subTree ps'
            | otherwise = mempty
    in  tag <> case branch of
      Tip -> mempty
      Bin l r | continue  -> query l ps <> query r ps
              | otherwise -> mempty

data Tail t

deriving instance Show t => Show (STree (Tail t))
deriving instance Show t => Show (Intv  (Tail t))

instance Eq (Intv (Tail t)) where
  (==) = const $ const True
instance Ord (Intv (Tail t)) where
  compare = const $ const EQ

instance Monoid t => SegmentTree (Tail t) where
  data STree     (Tail t) = TagTree t
  data Intv      (Tail t) = Tag { unTag :: t }
  type Point     (Tail t) = ()
  type MonoidTag (Tail t) = t

  fromList = TagTree . mconcat . map unTag
  query (TagTree t) () = t

-- Exported API

type SegmentTree2D a t = STree (a, (a, Tail t))
type SegmentTree1D a t = STree     (a, Tail t)

mkSegmentTree2D :: (Monoid t, Ord a) => [(((a,a),(a,a)),t)] -> SegmentTree2D a t
mkSegmentTree2D ranges =
  fromList [ Intv (closedInterval (x1,x2)) $
             Intv (closedInterval (y1,y2)) $
             Tag t
           | (((x1,x2),(y1,y2)),t) <- ranges
           ]

querySegmentTree2D :: (Monoid t, Ord a) => SegmentTree2D a t -> (a,a) -> t
querySegmentTree2D tree (x,y) = query tree (x,(y,()))

mkSegmentTree1D :: (Monoid t, Ord a) => [((a,a),t)] -> SegmentTree1D a t
mkSegmentTree1D ranges =
  fromList [ Intv (closedInterval (x1,x2)) $
             Tag t
           | ((x1,x2),t) <- ranges
           ]

querySegmentTree1D :: (Monoid t, Ord a) => SegmentTree1D a t -> a -> t
querySegmentTree1D tree x = query tree (x,())

-- Tests

_tree1 :: SegmentTree1D Int [Int]
_tree1 = mkSegmentTree1D
         [ ((1,10),[1])
         , ((3, 7),[2])
         ]

_tree2 :: SegmentTree2D Int [Int]
_tree2 = mkSegmentTree2D
         [ (((1,10),(1,10)),[1])
         , (((3, 7),(3, 7)),[2])
         ]

_query1 = [ (i,) $ querySegmentTree1D _tree1 i
          | i <- [0..11]]
_query2 = [ [ querySegmentTree2D _tree2 (i,j)
            | i <- [0..11]
            ]
          | j <- [0..11]
          ]

_main = do
  mapM_ (putStrLn . show) _query1
  mapM_ (putStrLn . show) _query2
