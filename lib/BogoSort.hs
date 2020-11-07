{-# LANGUAGE BangPatterns #-}

module BogoSort where

import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.ST
import           Data.Bits
import           Data.STRef.Strict
import           Data.Word
import           Unsafe.Coerce
import           Data.Time.Clock.POSIX             (getPOSIXTime)
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM
import qualified Data.Vector.Unboxed               as VU
import qualified Data.Vector.Unboxed.Mutable       as VUM

bogoSort :: (Ord a, VU.Unbox a) => VU.Vector a -> IO (VU.Vector a)
bogoSort = bogoSortBy compare

bogoSortBy :: (Ord a, VU.Unbox a) => (a -> a -> Ordering) -> VU.Vector a -> IO (VU.Vector a)
bogoSortBy compare vec = do
  rng <- newRNG
  fix $ \loop -> do
    if isSorted compare vec
      then return vec
      else do
        shuffle rng vec
        loop

isSorted :: (Ord a, VU.Unbox a) => (a -> a -> Ordering) -> VU.Vector a -> Bool
isSorted comp vec = runST $ do
  ret <- newSTRef True
  rep (VU.length vec - 1) $ \i -> when (comp (vec VU.! i) (vec VU.! (i + 1)) /= LT) $ writeSTRef ret False
  readSTRef ret

type RNG = VUM.IOVector Word64

getSeed :: IO Word64
getSeed = do
  t <- getPOSIXTime
  let ret = floor $ t * 1e9 :: Word64
  if ret == 0
    then getSeed
    else return ret

newRNG :: IO RNG
newRNG = do
  x <- getSeed
  VUM.replicate 1 (8172645463325252 - x)
{-# NOINLINE newRNG #-}

nextWord64 :: RNG -> IO Word64
nextWord64 rng = do
  x <- VUM.unsafeRead rng 0
  let
    y = x .<<. 7 .^. x
    z = y .>>. 9 .^. y
  VUM.unsafeWrite rng 0 z
  return z

shuffleM :: VUM.Unbox a => RNG -> VUM.IOVector a -> IO ()
shuffleM rng mvec = do
  rev (VUM.length mvec) $ \i -> do
    j <- nextWord64 rng
    VUM.unsafeSwap mvec i (unsafeCoerce $ rem j (unsafeCoerce i + 1))

shuffle :: VU.Unbox a => RNG -> VU.Vector a -> IO (VU.Vector a)
shuffle rng vec = do
  mv <- VU.unsafeThaw vec
  shuffleM rng mv
  VU.unsafeFreeze mv

infixl 8 .<<., .>>.
infixl 6 .^.

(.<<.) :: Bits b => b -> Int -> b
(.<<.) = unsafeShiftL
{-# INLINE (.<<.) #-}

(.>>.) :: Bits b => b -> Int -> b
(.>>.) = unsafeShiftR
{-# INLINE (.>>.) #-}

(.^.) :: Bits b => b -> b -> b
(.^.) = xor
{-# INLINE (.^.) #-}

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

streamR :: Monad m => Int -> Int -> VFSM.Stream m Int
streamR !l !r = VFSM.Stream step (r - 1)
  where
    step x
      | x >= l    = return $ VFSM.Yield x (x - 1)
      | otherwise = return $ VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] streamR #-}

rev :: Monad m => Int -> (Int -> m ()) -> m ()
rev n = flip VFSM.mapM_ (streamR 0 n)
{-# INLINE rev #-}
