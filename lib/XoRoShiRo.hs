{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}

module XoRoShiRo where

import           Data.Bits
import           GHC.Exts
import           Unsafe.Coerce
import           System.CPUTime
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM
import qualified Data.Vector.Unboxed               as VU
import qualified Data.Vector.Unboxed.Mutable       as VUM

type RNG = VUM.IOVector Word

getSeed :: IO Word
getSeed = do
  t <- getCPUTime
  return (fromInteger t * 0x73fe981a)

splitMix64 :: Word -> (Word, Word)
splitMix64 state = case state + 0x9e3779b97f4a7c15 of
  z0 -> case (z0 .^. z0 .>>. 30) * 0xbf58476d1ce4e5b9 of
    z1 -> case (z1 .^. z1 .>>. 27) * 0x94d049bb133111eb of
      z2 -> case z2 .^. z2 .>>. 31 of
        z3 -> (z0, z3)

newRNG :: IO RNG
newRNG = newRNGWithSeed =<< getSeed

newRNGWithSeed :: Word -> IO RNG
newRNGWithSeed seed = do
  let (s0, seed') = splitMix64 seed
  let (s1, _)     = splitMix64 seed'
  rng <- VUM.unsafeNew 2
  VUM.unsafeWrite rng 0 s0
  VUM.unsafeWrite rng 1 s1
  return rng

nextWord :: RNG -> IO Word
nextWord rng = do
  !s0 <-            VUM.unsafeRead rng 0
  !s1 <- xor s0 <$> VUM.unsafeRead rng 1
  VUM.unsafeWrite rng 0 $ (s0 .<<. 24  .|.  s0 .>>. 40) .^. s1 .^. (s1 .<<. 16)
  VUM.unsafeWrite rng 1 $ (s1 .<<. 37) .|. (s1 .>>. 27)
  let s05 = s0 * 5
  return $! (s05 .<<. 7 .|. s05 .>>. 57) * 9

nextInt :: RNG -> IO Int
nextInt rng = unsafeCoerce @Word @Int <$> nextWord rng

nextDouble :: RNG -> IO Double
nextDouble rng = do
  t <- nextWord rng
  let x = 0x3ff .<<. 52 .|. t .>>. 12
  return $! unsafeCoerce @Word @Double x - 1.0

nextGauss :: RNG -> Double -> Double -> IO Double
nextGauss rng mu sigma = do
  x <- nextDouble rng
  y <- nextDouble rng
  let z = sqrt (-2.0 * log x) * cos (2.0 * pi * y)
  return $! sigma * z + mu

withRNG :: (RNG -> IO a) -> IO a
withRNG f = do
  seed <- getSeed
  withRNGWithSeed seed f

withRNGWithSeed :: Word -> (RNG -> IO a) -> IO a
withRNGWithSeed seed f = newRNGWithSeed seed >>= f

shuffleM :: VUM.Unbox a => RNG -> VUM.IOVector a -> IO ()
shuffleM rng mvec = do
  rev (VUM.length mvec) $ \i -> do
    j <- nextWord rng
    VUM.unsafeSwap mvec i (unsafeCoerce $ rem j (unsafeCoerce i + 1))

shuffle :: VU.Unbox a => VU.Vector a -> IO (VU.Vector a)
shuffle vec = withRNG $ \rng -> do
  mv <- VU.unsafeThaw vec
  shuffleM rng mv
  VU.unsafeFreeze mv

randomR :: RNG -> Int -> Int -> IO Int
randomR rng l r = (+ l) . flip mod (r - l + 1) <$> nextInt rng

infixl 8 .<<., .>>., .>>>.
infixl 6 .^.

(.<<.) :: Bits b => b -> Int -> b
(.<<.) = unsafeShiftL
{-# INLINE (.<<.) #-}

(.>>.) :: Bits b => b -> Int -> b
(.>>.) = unsafeShiftR
{-# INLINE (.>>.) #-}

(.>>>.) :: Int -> Int -> Int
(I# x#) .>>>. (I# y#) = I# (uncheckedIShiftRL# x# y#)
{-# INLINE (.>>>.) #-}

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
