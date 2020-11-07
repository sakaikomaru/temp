{-# LANGUAGE BangPatterns #-}

module WarshallFloyd where

import           Control.Monad.ST
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM
import qualified Data.Vector.Unboxed               as VU
import qualified Data.Vector.Unboxed.Mutable       as VUM

warshallFloyd :: (VU.Unbox a, Num a, Ord a) => Int -> VUM.STVector s a -> ST s ()
warshallFloyd n d = do
  rep n $ \i -> VUM.unsafeWrite d (i * n + i) 0
  rep n $ \k -> rep n $ \i -> rep n $ \j -> do
    dij <- VUM.unsafeRead d (i * n + j)
    dik <- VUM.unsafeRead d (i * n + k)
    dkj <- VUM.unsafeRead d (k * n + j)
    VUM.unsafeWrite d (i * n + j) $ min dij (dik + dkj)

stream :: Monad m => Int -> Int -> VFSM.Stream m Int
stream !l !r = VFSM.Stream step l
  where
    step x
      | x < r     = return $ VFSM.Yield x (x + 1)
      | otherwise = return $ VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] stream #-}

rep :: Monad m => Int -> (Int -> m ()) -> m ()
rep n = flip VFSM.mapM_ (stream 0 n)
{-# INLINE rep #-}
