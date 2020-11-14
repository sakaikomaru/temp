{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}

module MersenneTwister where

import           Data.Bits
import           Data.Word
import           System.CPUTime
import           Unsafe.Coerce
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM 
import qualified Data.Vector.Unboxed               as VU
import qualified Data.Vector.Unboxed.Mutable       as VUM

_matrixA :: Word32
_matrixA = 0x9908b0df
{-# INLINE _matrixA #-}

_upperMask :: Word32
_upperMask = 0x80000000
{-# INLINE _upperMask #-}

_lowerMask :: Word32
_lowerMask = 0x7fffffff
{-# INLINE _lowerMask #-}

_pointer :: Int
_pointer = 624
{-# INLINE _pointer #-}

type Mt19937 = VUM.IOVector Word32

newRNG :: IO Mt19937
newRNG = do
  seed    <- (fromInteger :: Integer -> Word32) <$> getCPUTime
  mt19937 <- VUM.unsafeNew (625 :: Int)
  VUM.unsafeWrite mt19937 _pointer 0
  VUM.unsafeWrite mt19937 0 seed
  let
    set :: Int -> IO ()
    set mti = do
      o <- VUM.unsafeRead mt19937 (mti - 1)
      let v = 1812433253 * (o .^. (o .>>. 30)) + fromIntegral mti
      VUM.unsafeWrite mt19937 mti v
  mapM_ set [1..623]
  return mt19937

shiftAndXor :: Word32 -> Word32
shiftAndXor w0 =
  let
    w1 = w0 .^.  (w0 .>>. 11)
    w2 = w1 .^. ((w1 .<<. 7 ) .&. 0x9d2c5680)
    w3 = w2 .^. ((w2 .<<. 15) .&. 0xefc60000)
    w4 = w3 .^.  (w3 .>>. 18)
  in
    w4
{-# INLINE shiftAndXor #-}

nextWord32 :: Mt19937 -> IO Word32
nextWord32 mt19937 = do
  ptr <- VUM.unsafeRead mt19937 _pointer
  mti <- if ptr >= (624 :: Word32)
    then VUM.unsafeWrite mt19937 _pointer 0 >> step mt19937 >> return 0
    else VUM.unsafeModify mt19937 succ _pointer >> return ptr
  y   <- VUM.unsafeRead mt19937 (fromIntegral mti)
  return $ shiftAndXor y

nextWord64 :: Mt19937 -> IO Word64
nextWord64 mt19937 = do
  ptr <- VUM.unsafeRead mt19937 _pointer
  mti <- if ptr >= (623 :: Word32)
    then VUM.unsafeWrite mt19937 _pointer 0 >> step mt19937 >> return 0
    else VUM.unsafeModify mt19937 (+2) _pointer >> return ptr
  y <- VUM.unsafeRead mt19937 (fromIntegral mti)
  z <- VUM.unsafeRead mt19937 (fromIntegral mti + (1 :: Int))
  return $ (fromIntegral :: Word32 -> Word64) (shiftAndXor y) .<<. 32 .|. (fromIntegral :: Word32 -> Word64) (shiftAndXor z)

nextInt :: Mt19937 -> IO Int
nextInt = unsafeCoerce <$> nextWord64

nextWord :: Mt19937 -> IO Word
nextWord = unsafeCoerce <$> nextInt

nextDouble :: Mt19937 -> IO Double
nextDouble mt19937 = do
  t <- nextWord64 mt19937
  let x = 0x3ff .<<. 52 .|. t .>>. 12
  return $! unsafeCoerce @Word64 @Double x - 1.0

nextGauss :: Mt19937 -> Double -> Double -> IO Double
nextGauss mt19937 mu sigma = do
  x <- nextDouble mt19937
  y <- nextDouble mt19937
  let z = sqrt (-2.0 * log x) * cos (2.0 * pi * y)
  return $! sigma * z + mu

randomR :: Mt19937 -> (Int, Int) -> IO Int
randomR mt19937 (l, r) = flip mod (r - l + 1) <$> nextInt mt19937

step :: Mt19937 -> IO ()
step mt = do
  let
    mag01 :: Word32 -> Word32
    mag01 x = if odd x then _matrixA else 0
    set :: Int -> IO ()
    set kk = do
      mtkk  <- VUM.unsafeRead mt kk
      mtkk1 <- VUM.unsafeRead mt ((kk + 1) `mod` (624 :: Int))
      mtkkm <- VUM.unsafeRead mt ((kk + 397) `mod` (624 :: Int))
      let y = (mtkk .&. _upperMask) .|. (mtkk1 .&. _lowerMask)
          v = mtkkm .^. (y .>>. 1) .^. mag01 y
      VUM.unsafeWrite mt kk v
  mapM_ set [0..623]

shuffleM :: VUM.Unbox a => Mt19937 -> VUM.IOVector a -> IO ()
shuffleM mt19937 mvec = do
  rev (VUM.length mvec) $ \i -> do
    j <- nextWord64 mt19937
    VUM.unsafeSwap mvec i (unsafeCoerce $ rem j (unsafeCoerce i + 1))

shuffle :: VU.Unbox a => Mt19937 -> VU.Vector a -> IO (VU.Vector a)
shuffle mt19937 vec = do
  mv <- VU.unsafeThaw vec
  shuffleM mt19937 mv
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