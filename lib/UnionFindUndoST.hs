{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}

module UnionFindUndoST where

import           Control.Monad
import           Control.Monad.ST
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM
import qualified Data.Vector.Unboxed               as VU
import qualified Data.Vector.Unboxed.Mutable       as VUM

data UnionFindUndo s = UFU
  { valData :: VUM.STVector s Int
  , history :: VecStack s
  }

newUFU :: Int -> ST s (UnionFindUndo s)
newUFU n = UFU <$> VUM.replicate n (-1 :: Int) <*> newVecStack defaultVecStackSize

uniteUFU :: UnionFindUndo s -> Int -> Int -> ST s Bool
uniteUFU ufu@(UFU info his) x y = do
  x'    <- findUFU ufu x
  y'    <- findUFU ufu y
  datax <- VUM.unsafeRead info x'
  datay <- VUM.unsafeRead info y'
  pushVS (x', datax) his
  pushVS (y', datay) his
  if x' == y'
    then return False
    else do
      let (xx, yy) = if datax > datay then (y', x') else (x', y')
      datayy <- VUM.unsafeRead info yy
      VUM.unsafeModify info (+ datayy) xx
      VUM.unsafeWrite info yy xx
      return True

connectedUFU :: UnionFindUndo s -> Int -> Int -> ST s Bool
connectedUFU ufu x y = liftM2 (==) (findUFU ufu x) (findUFU ufu y)
{-# INLINE connectedUFU #-}

findUFU :: UnionFindUndo s -> Int -> ST s Int
findUFU ufu@(UFU info _) k = do
  datak <- VUM.unsafeRead info k
  if datak < 0 then return k
  else findUFU ufu datak

sizeUFU :: UnionFindUndo s -> Int -> ST s Int
sizeUFU ufu@(UFU info _) k = do
  findk <- findUFU ufu k
  ans <- VUM.unsafeRead info findk
  return (-ans)

undoUFU :: UnionFindUndo s -> ST s ()
undoUFU (UFU info his) = do
  popVS his >>= \case
    Nothing     -> return ()
    Just (x, y) -> VUM.unsafeWrite info x y
  popVS his >>= \case
    Nothing     -> return ()
    Just (z, w) -> VUM.unsafeWrite info z w

snapshotUFU :: UnionFindUndo s -> ST s ()
snapshotUFU (UFU _ his) = do
  sz <- sizeVS his
  when (sz > 0) $ rep sz $ \_ -> void $ popVS his

rollbackUFU :: UnionFindUndo s -> ST s ()
rollbackUFU ufu@(UFU _ his) = do
  sz <- sizeVS his
  when (sz > 0) $ rep sz $ \_ -> undoUFU ufu

data VecStack s = VecStack
  { intVarsVS  :: !(VUM.MVector s Int)
  , internalVS :: !(VUM.MVector s (Int, Int))
  }

_sizeVS :: Int
_sizeVS = 0
{-# INLINE _sizeVS #-}

sizeVS :: VecStack s -> ST s Int
sizeVS (VecStack info _) = VUM.unsafeRead info _sizeVS
{-# INLINE sizeVS #-}

newVecStack :: Int -> ST s (VecStack s)
newVecStack n = VecStack <$> VUM.replicate 1 0 <*> VUM.unsafeNew n

defaultVecStackSize :: Int
defaultVecStackSize = 1024 * 1024

topVS :: VecStack s -> ST s (Int, Int)
topVS (VecStack info s) = do
  len <- VUM.unsafeRead info _sizeVS
  if len > 0 then VUM.unsafeRead s (len - 1) else return (-1, -1)
{-# INLINE topVS #-}

popVS :: VecStack s -> ST s (Maybe (Int, Int))
popVS (VecStack info s) = do
  len <- VUM.unsafeRead info _sizeVS
  if len > 0
    then do
      VUM.unsafeWrite info _sizeVS (len - 1)
      pure <$> VUM.unsafeRead s (len - 1)
    else return Nothing
{-# INLINE popVS #-}

pushVS :: (Int, Int) -> VecStack s -> ST s ()
pushVS x (VecStack info s) = do
  len <- VUM.unsafeRead info _sizeVS
  VUM.unsafeWrite s len x
  VUM.unsafeWrite info _sizeVS (len + 1)
{-# INLINE pushVS #-}

pushesVS :: VU.Vector (Int, Int) -> VecStack s -> ST s ()
pushesVS vec (VecStack info s) = do
  len <- VUM.unsafeRead info _sizeVS
  VUM.unsafeWrite info _sizeVS (len + VU.length vec)
  VU.unsafeCopy (VUM.unsafeSlice len (VU.length vec) s) vec
{-# INLINE pushesVS #-}

freezeVecStack :: VecStack s -> ST s (VU.Vector (Int, Int))
freezeVecStack (VecStack info s) = do
  l <- VUM.unsafeRead info _sizeVS
  VU.unsafeFreeze $ VUM.take l s
{-# INLINE freezeVecStack #-}

stream :: Monad m => Int -> Int -> VFSM.Stream m Int
stream !l !r = VFSM.Stream step l
  where
    step x
      | x < r     = return $ VFSM.Yield x (x + 1)
      | otherwise = return VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] stream #-}

rep :: Monad m => Int -> (Int -> m ()) -> m ()
rep n = flip VFSM.mapM_ (stream 0 n)