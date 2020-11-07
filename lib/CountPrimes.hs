{-# LANGUAGE BangPatterns #-}

module CountPrimes where

import           Control.Monad
import           Control.Monad.ST
import           Data.Bits
import           Data.STRef
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM
import qualified Data.Vector.Unboxed.Mutable       as VUM

{-
10 ^  6 = 0.0003[s]        78498
10 ^  7 = 0.001 [s]       664579
10 ^  8 = 0.005 [s]      5761455
10 ^  9 = 0.022 [s]     50847534
10 ^ 10 = 0.110 [s]    455052511
10 ^ 11 = 0.551 [s]   4118054813
10 ^ 12 = 2.830 [s]  37607912018
10 ^ 13 = 13.27 [s] 346065536839
-}

countPrimes :: Int -> Int
countPrimes n = runST $ do
  higher <- VUM.replicate (v + 2) (0 :: Int)
  lower  <- VUM.replicate (v + 2) (0 :: Int)
  used   <- VUM.replicate (v + 2) False
  ret    <- newSTRef (n - 1)
  rep 2 v 1 $ \p -> do
    VUM.unsafeWrite lower p (p - 1)
    VUM.unsafeWrite higher p (n `div` p - 1)
  rep 2 v 1 $ \p -> do
    lowp  <- VUM.unsafeRead lower p
    lowp1 <- VUM.unsafeRead lower (p - 1)
    when (lowp /= lowp1) $ do
      highp <- VUM.unsafeRead higher p
      modifySTRef' ret (subtract (highp - lowp1))
      let
        pSquare = p * p
        end     = min v (n `div` pSquare)
        j       = 1 + (p .&. 1)
      rep (p + j) (end + 1) j $ \i -> do
        usedi <- VUM.unsafeRead used i
        unless usedi $ do
          let d = i * p
          if d <= v
            then do
              highd <- VUM.unsafeRead higher d
              VUM.unsafeModify higher (subtract (highd - lowp1)) i
            else do
              lownd <- VUM.unsafeRead lower (n `div` d)
              VUM.unsafeModify higher (subtract (lownd - lowp1)) i
      rev pSquare v $ \i -> do
        lowip <- VUM.unsafeRead lower (i `div` p)
        VUM.unsafeModify lower (subtract (lowip - lowp1)) i
      rep pSquare end (p * j) $ \i -> VUM.unsafeWrite used i True
  readSTRef ret
  where
    !v = floor . sqrt . fromIntegral $ n

stream :: Monad m => Int -> Int -> Int -> VFSM.Stream m Int
stream !l !r !d = VFSM.Stream step l
  where
    step x
      | x <= r    = return $ VFSM.Yield x (x + d)
      | otherwise = return $ VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] stream #-}

rep :: Monad m => Int -> Int -> Int -> (Int -> m ()) -> m ()
rep l r d = flip VFSM.mapM_ (stream l r d)
{-# INLINE rep #-}

streamR :: Monad m => Int -> Int -> VFSM.Stream m Int
streamR !l !r = VFSM.Stream step r
  where
    step x
      | x >= l    = return $ VFSM.Yield x (x - 1)
      | otherwise = return $ VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] streamR #-}

rev :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
rev l r = flip VFSM.mapM_ (streamR l r)
{-# INLINE rev #-}