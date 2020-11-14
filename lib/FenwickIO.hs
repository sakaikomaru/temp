{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module FenwickIO where

import           Control.Monad
import           Control.Monad.Fix
import           Data.Bits
import qualified Data.Vector.Unboxed               as VU
import qualified Data.Vector.Unboxed.Mutable       as VUM

type BIT = VUM.IOVector Int

newBIT :: Int -> IO BIT
newBIT n = VUM.replicate (n + 1) 0
{-# INLINE newBIT #-}

buildBIT :: VU.Vector Int -> IO BIT
buildBIT vec = do
  let n = VU.length vec
  bit <- VUM.unsafeNew (n + 1)
  VUM.unsafeWrite bit 0 0
  VU.unsafeCopy (VUM.tail bit) vec
  flip fix 1 $ \loop !i -> when (i <= n) $ do
    let j = i + (i .&. (-i))
    when (j <= n) $ do
      biti <- VUM.unsafeRead bit i
      VUM.unsafeModify bit (+ biti) j
    loop (i + 1)
  return bit
{-# INLINE buildBIT #-}

infixl 9 +++!, +++!!

-- | 0-indexed
(+++!!) :: BIT -> Int -> IO Int
bit +++!! i = bit +++! (i + 1)

-- | 1-indexed
(+++!) :: BIT -> Int -> IO Int
(+++!) bit = go 0
  where
    go !acc !i
      | i > 0 = do
          xi <- VUM.unsafeRead bit i
          go (acc + xi) (i - (i .&. (-i)))
      | otherwise = return acc

-- | 0-indexed
incBIT' :: BIT -> Int -> Int -> IO ()
incBIT' bit key val = flip fix (key + 1) $ \loop !i -> do
  when (i < n) $ do
    VUM.unsafeModify bit (+ val) i
    loop $ i + (i .&. (-i))
  where !n = VUM.length bit
{-# INLINE incBIT' #-}

-- | 1-indexed
incBIT :: BIT -> Int -> Int -> IO ()
incBIT bit key val = flip fix key $ \loop !i -> do
  when (i < n) $ do
    VUM.unsafeModify bit (+ val) i
    loop $ i + (i .&. (-i))
  where !n = VUM.length bit
{-# INLINE incBIT #-}

-- | 1-indexed [l, r)
sumFromTo :: BIT -> Int -> Int -> IO Int
sumFromTo bit l r = (-) <$> bit +++! (r - 1) <*> bit +++! (l - 1)
{-# INLINE sumFromTo #-}

-- | 1-indexed [l, r]
sumFromTo' :: BIT -> Int -> Int -> IO Int
sumFromTo' bit l r = (-) <$> bit +++! r <*> bit +++! (l - 1)
{-# INLINE sumFromTo' #-}

-- | 0-indexed
readBIT' :: BIT -> Int -> IO Int
readBIT' bit i = (-) <$> bit +++! (i + 1) <*> bit +++! i
{-# INLINE readBIT' #-}

-- | 1-indexed
readBIT :: BIT -> Int -> IO Int
readBIT bit i = (-) <$> bit +++! i <*> bit +++! (i - 1)
{-# INLINE readBIT #-}

-- | 0-indexed
writeBIT' :: BIT -> Int -> Int -> IO ()
writeBIT' bit i x = readBIT' bit i >>= incBIT' bit i . (x - )
{-# INLINE writeBIT' #-}

-- | 1-indexed
writeBIT :: BIT -> Int -> Int -> IO ()
writeBIT bit i x = readBIT bit i >>= incBIT bit i . (x - )
{-# INLINE writeBIT #-}

-- | 1-indexed
findMinIndexGT :: BIT -> Int -> IO Int
findMinIndexGT bit w0
  | w0 <= 0   = return 0
  | otherwise = do
    let n = VUM.length bit
    wmax <- bit +++! n
    if w0 > wmax
      then return (n + 1)
      else go w0 (floorPow2 n) 0 n
  where
    go !w !step !i !m
      | step == 0 = return (i + 1)
      | otherwise = do
        if i + step < m
          then do
            u <- VUM.unsafeRead bit (i + step)
            if u < w
              then go (w - u) (step .>>. 1) (i + step) m
              else go w (step .>>. 1) i m
          else go w (step .>>. 1) i m
{-# INLINE findMinIndexGT #-}

infixl 8 .<<., .>>.

(.<<.) :: Bits b => b -> Int -> b
(.<<.) = unsafeShiftL
{-# INLINE (.<<.) #-}

(.>>.) :: Bits b => b -> Int -> b
(.>>.) = unsafeShiftR
{-# INLINE (.>>.) #-}

clz :: FiniteBits fb => fb -> Int
clz = countLeadingZeros
{-# INLINE clz #-}

floorPow2 :: Int -> Int
floorPow2 n
  | n >= 1    = 1 .<<. (63 - clz n)
  | otherwise = 0