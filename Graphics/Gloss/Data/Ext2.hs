{-# language
   RecordWildCards
 #-}

module Graphics.Gloss.Data.Ext2
 ( Ext2
 , Ext
 , translateExt2
 , scaleExt2
 , fixSizeExt2
 , flattenExt2
 , mkWeakExt
 , pointInExt2
 ) where

import Data.Monoid

import Graphics.Gloss.Data.Ext

data Ext2 = Ext2 { strongExt :: Ext
                 , weakExt :: Ext
                 }
  deriving (Eq, Show)

flattenExt2 :: Ext2 -> Ext
flattenExt2 Ext2{..} = strongExt <> weakExt

instance Monoid Ext2 where
  mempty = Ext2 { strongExt = mempty
                , weakExt = mempty
                }
  mappend e1 e2 = Ext2 { strongExt = strongExt e1 <> strongExt e2
                       , weakExt = weakExt e1 <> weakExt e2
                       }

onWeakExt :: (Ext -> Ext) -> Ext2 -> Ext2
onWeakExt f e@Ext2{..} = e { weakExt = f weakExt }

translateExt2 :: Float -> Float -> Ext2 -> Ext2
translateExt2 x y = onWeakExt (translateExt x y)

scaleExt2 :: Float -> Float -> Ext2 -> Ext2
scaleExt2 x y = onWeakExt (scaleExt x y)

mkWeakExt :: Ext -> Ext2
mkWeakExt e = mempty { weakExt = e }

mkStrongExt :: Ext -> Ext2
mkStrongExt e = mempty { strongExt = e }

fixSizeExt2 :: Maybe Float -> Maybe Float -> Ext2 -> Ext2
fixSizeExt2 sizew sizeh Ext2{..} =
  let strongExt' = resizeExt sizew sizeh weakExt
  in  mkStrongExt $ strongExt <> strongExt'
  where
    resizeExt :: Maybe Float -> Maybe Float -> Ext -> Ext
    resizeExt mw mh ext = case getExt ext of
      Nothing -> ext
      Just (_,(ex,ey)) ->
        case (mw, mh) of
          (Nothing, Nothing) -> ext
          (Just w,  Nothing) -> let ratio = w / we
                                in  enlargeExt ratio ratio ext
          (Nothing,  Just h) -> let ratio = h / he
                                in  enlargeExt ratio ratio ext
          (Just w,   Just h) -> let ratioh = h / he
                                    ratiow = w / we
                                in  enlargeExt ratiow ratioh ext
          where
            we = ex * 2
            he = ey * 2

pointInExt2 :: Ext2 -> (Float,Float) -> (Float,Float) -> Bool
pointInExt2 Ext2{..} realP transP =
  pointInExt strongExt realP ||
  pointInExt   weakExt transP
