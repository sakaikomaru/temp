{-# LANGUAGE BangPatterns #-}

module Montmort where

import qualified Data.Vector.Fusion.Stream.Monadic as VFSM
import qualified Data.Vector.Unboxed               as VU
import qualified Data.Vector.Unboxed.Mutable       as VUM

montmort :: Int -> Int -> IO (VU.Vector Int)
montmort n mo = do
  dp <- VU.unsafeThaw $ VU.replicate (n + 1) 0
  rep (n - 1) $ \k -> do
    item <- VUM.unsafeRead dp (k + 1)
    if odd k
      then VUM.unsafeWrite dp (k + 2) (mod (item * (k + 2) - 1) mo)
      else VUM.unsafeWrite dp (k + 2) (mod (item * (k + 2) + 1) mo)
  VU.unsafeFreeze dp

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