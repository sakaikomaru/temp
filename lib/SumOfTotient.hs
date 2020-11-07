{-# LANGUAGE BangPatterns #-}

module SumOfTotient where

import           Control.Monad
import           Control.Monad.ST
import           Data.STRef
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM
import qualified Data.Vector.Mutable               as VM
import qualified Data.Vector.Unboxed.Mutable       as VUM

modulo :: Integer
modulo = 998244353
{-# INLINE modulo #-}

sumOfTotient :: Int -> Integer
sumOfTotient n = runST $ do
  phi <- VUM.replicate (v + 1) (1 :: Int)
  rep 2 v $ \i -> do
    phiI <- VUM.unsafeRead phi i
    when (phiI == 1) $ do
      repS i v i $ \j -> VUM.unsafeModify phi (* (i - 1)) j
      repM (i * i) v i $ \q -> repS q v q $ \j -> VUM.unsafeModify phi (* i) j 
  sumS <- VUM.unsafeNew (v + 1) :: ST s (VUM.STVector s Int)
  rep 1 v $ \i -> do
    item1 <- VUM.unsafeRead phi i
    item2 <- VUM.unsafeRead sumS (i - 1)
    VUM.unsafeWrite sumS i (item1 + item2)
  sumL <- VM.replicate (v + 1) (0 :: Integer)
  rev1 v $ \d -> do
    let
      m     = n `div` d
      sqrtm = floor . sqrt . fromIntegral $ m
      th    = v `div` d
    total <- newSTRef (fi m * (fi (m + 1)) `div` 2)
    rep 1 th $ \i -> do
      item1 <- VM.unsafeRead sumL (d * i)
      item2 <- VUM.unsafeRead phi i
      modifySTRef' total (subtract (item1 + (fi item2) * fi (m `div` i)))
    rep (th + 1) sqrtm $ \i -> do
      let
        t = m `div` i
      item1 <- VUM.unsafeRead sumS t
      item2 <- VUM.unsafeRead phi i
      modifySTRef' total (subtract ((fi item1) + (fi item2) * (fi t)))
    item1 <- readSTRef total
    item2 <- VUM.unsafeRead sumS sqrtm
    VM.unsafeWrite sumL d (item1 + (fi item2) * (fi sqrtm)) 
  VM.unsafeRead sumL 1
  where
    !v = floor . sqrt . fromIntegral $ n

fi :: Int -> Integer
fi = fromIntegral
{-# INLINE fi #-}

fI :: Integer -> Int
fI = fromInteger
{-# INLINE fI #-}

stream :: Monad m => Int -> Int -> VFSM.Stream m Int
stream !l !r = VFSM.Stream step l
  where
    step x
      | x <= r    = return $ VFSM.Yield x (x + 1)
      | otherwise = return $ VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] stream #-}

rep :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
rep l r = flip VFSM.mapM_ (stream l r)
{-# INLINE rep #-}

streamS :: Monad m => Int -> Int -> Int -> VFSM.Stream m Int
streamS !l !r !s = VFSM.Stream step l
  where
    step x
      | x <= r    = return $ VFSM.Yield x (x + s)
      | otherwise = return $ VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] streamS #-}

repS :: Monad m => Int -> Int -> Int -> (Int -> m ()) -> m ()
repS l r s = flip VFSM.mapM_ (streamS l r s)
{-# INLINE repS #-}

streamM :: Monad m => Int -> Int -> Int -> VFSM.Stream m Int
streamM !l !r !s = VFSM.Stream step l
  where
    step x
      | x <= r    = return $ VFSM.Yield x (x * s)
      | otherwise = return $ VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] streamM #-}

repM :: Monad m => Int -> Int -> Int -> (Int -> m ()) -> m ()
repM l r s = flip VFSM.mapM_ (streamM l r s)
{-# INLINE repM #-}

streamR :: Monad m => Int -> Int -> VFSM.Stream m Int
streamR !l !r = VFSM.Stream step r
  where
    step x
      | x >= l    = return $ VFSM.Yield x (x - 1)
      | otherwise = return $ VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] streamR #-}

rev1 :: Monad m => Int -> (Int -> m ()) -> m ()
rev1 n = flip VFSM.mapM_ (streamR 1 n)
{-# INLINE rev1 #-}
