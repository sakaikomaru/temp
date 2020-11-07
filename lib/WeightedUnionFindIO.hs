module WeightedUnionFindIO where

import qualified Data.Vector.Unboxed.Mutable as VUM

data WeightedUnionFind a = WUF
  { valData :: VUM.IOVector Int
  , weight  :: VUM.IOVector a
  }

newWUF :: (Eq a, Num a, VUM.Unbox a) => Int -> IO (WeightedUnionFind a)
newWUF n = WUF <$> VUM.replicate n (-1) <*> VUM.unsafeNew n

findWUF :: (Eq a, Num a, VUM.Unbox a) => WeightedUnionFind a -> Int -> IO Int
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

weightWUF :: (Eq a, Num a, VUM.Unbox a) => WeightedUnionFind a -> Int -> IO a
weightWUF wuf@(WUF _ ws) t = findWUF wuf t >> VUM.unsafeRead ws t

uniteWUF :: (Eq a, Num a, VUM.Unbox a) => WeightedUnionFind a -> Int -> Int -> a -> IO Bool
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

diffWUF :: (Eq a, Num a, VUM.Unbox a) => WeightedUnionFind a -> Int -> Int -> IO a
diffWUF (WUF _ ws) x y = do
  wx <- VUM.unsafeRead ws x
  wy <- VUM.unsafeRead ws y
  return $ wy - wx
