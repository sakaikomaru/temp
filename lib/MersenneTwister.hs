{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE BinaryLiterals   #-}
{-# LANGUAGE TypeApplications #-}

module MersenneTwister where

import           Control.Monad
import           Data.Bits
import           Data.Word
import           Unsafe.Coerce

import qualified Data.Vector.Fusion.Stream.Monadic as VFSM
import qualified Data.Vector.Unboxed               as VU
import qualified Data.Vector.Unboxed.Mutable       as VUM

{- https://en.wikipedia.org/wiki/Mersenne_Twister -}

_pointer :: Int
_pointer = 312
{-# INLINE _pointer #-}

_lowerMask :: Word64
_lowerMask = 0b0000000000000000000000000000000001111111111111111111111111111111
{-# INLINE _lowerMask #-}

_upperMask :: Word64
_upperMask = 0b1111111111111111111111111111111110000000000000000000000000000000
{-# INLINE _upperMask #-}

type MT19937 = VUM.IOVector Word64

-- 0 .. 311 data
--         312 ptr

newMT19937 :: Word64 -> IO MT19937
newMT19937 seed = do
  mt <- VUM.unsafeNew 313 :: IO MT19937
  VUM.unsafeWrite mt _pointer 0
  VUM.unsafeWrite mt 0 seed
  flip VFSM.mapM_ (stream 1 311 1) $ \mti -> do
    item <- VUM.unsafeRead mt (mti - 1)
    let rnd = 6364136223846793005 * (item .^. (item .>>. 62)) + unsafeCoerce @Int @Word64 mti
    VUM.unsafeWrite mt mti rnd
  return mt

shiftAndXor :: Word64 -> Word64
shiftAndXor w0 =
  case w0 .^. ((w0 .>>. 29) .&. 0x5555555555555555) of
    w1 -> case w1 .^. ((w1 .<<. 17) .&. 0x71D67FFFEDA60000) of
      w2 -> case w2 .^. ((w2 .<<. 37) .&. 0xFFF7EEE000000000) of
        w3 -> w3 .^. (w3 .>>. 43)

twist :: MT19937 -> IO ()
twist mt = do
  rep 312 $ \i -> do
    item1 <- VUM.unsafeRead mt i
    item2 <- VUM.unsafeRead mt ((i + 1) `mod` 312)
    let
      x  = (item1 .&. _upperMask) + (item2 .&. _lowerMask)
      xA = x .>>. 1
      xa  = if odd x then xA .^. 0xB5026F5AA96619E9 else xA
    item3 <- VUM.unsafeRead mt ((i + 156) `mod` 312)
    VUM.unsafeWrite mt i (item3 .^. xa)
  VUM.unsafeWrite mt _pointer 0

nextWord64 :: MT19937 -> IO Word64
nextWord64 mt = do
  idx <- VUM.unsafeRead mt _pointer
  when (idx >= 312) $ twist mt
  y <- shiftAndXor <$> VUM.unsafeRead mt (fromIntegral idx)
  VUM.unsafeModify mt succ _pointer
  return y

nextInt :: MT19937 -> IO Int
nextInt mt = unsafeCoerce <$> nextWord64 mt

nextWord :: MT19937 -> IO Word
nextWord mt = unsafeCoerce <$> nextWord64 mt

nextDouble :: MT19937 -> IO Double
nextDouble mt19937 = do
  t <- nextWord64 mt19937
  let x = 0x3ff .<<. 52 .|. t .>>. 12
  return $! unsafeCoerce @Word64 @Double x - 1.0

nextGauss :: MT19937 -> Double -> Double -> IO Double
nextGauss mt19937 mu sigma = do
  x <- nextDouble mt19937
  y <- nextDouble mt19937
  let z = sqrt (-2.0 * log x) * cos (2.0 * pi * y)
  return $! sigma * z + mu

randomR :: MT19937 -> Int -> Int -> IO Int
randomR mt19937 l r = (+ l) . flip mod (r - l + 1) <$> nextInt mt19937

shuffleM :: VUM.Unbox a => MT19937 -> VUM.IOVector a -> IO ()
shuffleM mt19937 mvec = do
  rev (VUM.length mvec) $ \i -> do
    j <- nextWord64 mt19937
    VUM.unsafeSwap mvec i (unsafeCoerce $ rem j (unsafeCoerce i + 1))

shuffle :: VU.Unbox a => MT19937 -> VU.Vector a -> IO (VU.Vector a)
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

-- for (int x = l; x <= r; x += d)
stream :: Monad m => Int -> Int -> Int -> VFSM.Stream m Int
stream !l !r !d = VFSM.Stream step l
  where
    step x
      | x <= r    = return $ VFSM.Yield x (x + d)
      | otherwise = return VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] stream #-}

rep :: Monad m => Int -> (Int -> m ()) -> m ()
rep n = flip VFSM.mapM_ (stream 0 (n - 1) 1)
{-# INLINE rep #-}

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
