{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE TypeApplications #-}

module XoShi where

import           Data.Bits
import           Data.Word
import           System.CPUTime
import           Unsafe.Coerce
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM
import qualified Data.Vector.Unboxed               as VU
import qualified Data.Vector.Unboxed.Mutable       as VUM

type RNG = VUM.IOVector Word64

getSeed :: IO Word64
getSeed = do
  t <- getCPUTime
  return (fromInteger t * 0x3b47f24a)

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

nextInt :: RNG -> IO Int
nextInt rng = unsafeCoerce <$> nextWord64 rng

nextDouble :: RNG -> IO Double
nextDouble rng = do
  t <- nextWord64 rng
  let x = 0x3ff .<<. 52 .|. t .>>. 12
  return $! unsafeCoerce @Word64 @Double x - 1.0

nextGauss :: RNG -> Double -> Double -> IO Double
nextGauss rng mu sigma = do
  x <- nextDouble rng
  y <- nextDouble rng
  let z = sqrt (-2.0 * log x) * cos (2.0 * pi * y)
  return $! sigma * z + mu

randomR :: RNG -> Int -> Int -> IO Int
randomR rng l r = do
  x <- nextInt rng
  return $ x `mod` (r - l + 1) + l

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

streamR :: Monad m => Int -> Int -> VFSM.Stream m Int
streamR !l !r = VFSM.Stream step (r - 1)
  where
    step x
      | x >= l    = return $ VFSM.Yield x (x - 1)
      | otherwise = return VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] streamR #-}

rev :: Monad m => Int -> (Int -> m ()) -> m ()
rev n = flip VFSM.mapM_ (streamR 0 n)
{-# INLINE rev #-}
