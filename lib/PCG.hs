{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE TypeApplications #-}

module PCG where

import           Data.Bits
import           Data.Word
import           GHC.Exts
import           System.CPUTime
import           Unsafe.Coerce
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM
import qualified Data.Vector.Unboxed               as VU
import qualified Data.Vector.Unboxed.Mutable       as VUM

seed :: Word64
seed = 0x4d595df4d0f33173
{-# INLINE seed #-}

multiplier :: Word64
multiplier = 0x5851f42d4c957f2d
{-# INLINE multiplier #-}

increment :: Word64
increment = 0x14057b7ef767814f
{-# INLINE increment #-}

type RNG = VUM.IOVector Word64

newRNG :: IO RNG
newRNG = do
  t <- fromInteger <$> getCPUTime
  VUM.replicate 1 (seed + t)

nextWord32 :: RNG -> IO Word32
nextWord32 rng = do
  x1 <- VUM.unsafeRead rng 0
  let
    cnt   = x1 .>>. 59
    state = x1 * multiplier + increment
    x2    = x1 .^. (x1 .>>. 18)
  VUM.unsafeWrite rng 0 state
  return $ unsafeCoerce (x2 .>>. 27) .>>@. unsafeCoerce @Word64 @Int cnt

nextWord :: RNG -> IO Word
nextWord rng = do
  w32 <- nextWord32 rng
  let
    w64 :: Word
    w64 = unsafeCoerce w32
  return $ (w64 .<<. 48) .|. (w64 .<<. 16) .|. (w64 .>>. 16)

nextInt :: RNG -> IO Int
nextInt = unsafeCoerce <$> nextWord

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

randomR :: RNG -> Int -> Int -> IO Int
randomR rng l r = (+ l) . flip mod (r - l + 1) <$> nextInt rng

shuffleM :: VUM.Unbox a => RNG -> VUM.IOVector a -> IO ()
shuffleM rng mvec = do
  rev (VUM.length mvec) $ \i -> do
    j <- nextWord rng
    VUM.unsafeSwap mvec i (unsafeCoerce $ rem j (unsafeCoerce i + 1))

shuffle :: VU.Unbox a => RNG -> VU.Vector a -> IO (VU.Vector a)
shuffle rng vec = do
  mv <- VU.unsafeThaw vec
  shuffleM rng mv
  VU.unsafeFreeze mv

infixr 8 .>>@.
infixl 8 .<<., .>>., .>>>.
infixl 6 .^.

(.<<.) :: Bits b => b -> Int -> b
(.<<.) = unsafeShiftL
{-# INLINE (.<<.) #-}

(.>>.) :: Bits b => b -> Int -> b
(.>>.) = unsafeShiftR
{-# INLINE (.>>.) #-}

(.>>>.) :: Int -> Int -> Int
(.>>>.) (I# x#) (I# i#) = I# (uncheckedIShiftRL# x# i#)
{-# INLINE (.>>>.) #-}

(.^.) :: Bits b => b -> b -> b
(.^.)  = xor
{-# INLINE (.^.)  #-}

(.>>@.) :: Word32 -> Int -> Word32
x .>>@. k = x .>>. k .|. x .<<. ((-k) .&. 31)
{-# INLINE (.>>@.) #-}

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
