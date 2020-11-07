module WeightedUnionFindST where

import           Control.Monad.ST
import qualified Data.Vector.Unboxed.Mutable as VUM

data WeightedUnionFind s a = WUF
  { valData :: VUM.STVector s Int
  , weight  :: VUM.STVector s a
  }

newWUF :: (Eq a, Num a, VUM.Unbox a) => Int -> ST s (WeightedUnionFind s a)
newWUF n = WUF <$> VUM.replicate n (-1) <*> VUM.unsafeNew n

findWUF :: (Eq a, Num a, VUM.Unbox a) => WeightedUnionFind s a -> Int -> ST s Int
findWUF wuf@(WUF val ws) k = do
  valk <- VUM.unsafeRead val k
  if valk < 0
    then return k
    else do
      par     <- findWUF wuf valk
      wsdatak <- VUM.unsafeRead ws valk
      VUM.unsafeModify ws (+ wsdatak) k
      VUM.unsafeWrite val k par
      return par

weightWUF :: (Eq a, Num a, VUM.Unbox a) => WeightedUnionFind s a -> Int -> ST s a
weightWUF wuf@(WUF _ ws) t = findWUF wuf t >> VUM.unsafeRead ws t

uniteWUF :: (Eq a, Num a, VUM.Unbox a) => WeightedUnionFind s a -> Int -> Int -> a -> ST s Bool
uniteWUF wuf@(WUF val ws) x y w'' = do
  itemW1<- VUM.unsafeRead ws x
  itemW2<- VUM.unsafeRead ws y
  let w' = w'' + itemW1 - itemW2
  x' <- findWUF wuf x
  y' <- findWUF wuf y
  if x' == y'
    then return False
    else do
      datax <- VUM.unsafeRead val x'
      datay <- VUM.unsafeRead val y'
      let (xx, yy) = if datax > datay then (y', x') else (x', y')
      let w = if datax > datay then - w' else w'
      datayy <- VUM.unsafeRead val yy
      VUM.unsafeModify val (+ datayy) xx
      VUM.unsafeWrite val yy xx
      VUM.unsafeWrite ws yy w
      return True

diffWUF :: (Eq a, Num a, VUM.Unbox a) => WeightedUnionFind s a -> Int -> Int -> ST s a
diffWUF (WUF _ ws) x y = do
  wx <- VUM.unsafeRead ws x
  wy <- VUM.unsafeRead ws y
  return $ wy - wx