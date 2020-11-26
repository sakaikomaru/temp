{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections     #-}

module RadixSort where

import           Data.Bits
import qualified Data.Foldable               as F
import           Data.Word
import           Unsafe.Coerce
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

radixSort64 :: VU.Vector Word64 -> VU.Vector Word64
radixSort64 vword = F.foldl' step vword ([0,16,32,48] :: [Int])
  where
    mask k x = fromIntegral $ x .>>. k .&. 0xffff
    step v k = VU.create $ do
      pref <- VU.unsafeThaw
            . VU.prescanl' (+) 0
            . VU.unsafeAccumulate (+) (VU.replicate 0x10000 0)
            $ VU.map ((, 1) . mask k) v
      res <- VUM.unsafeNew $ VU.length v
      VU.forM_ v $ \x -> do
        let !masked = mask k x
        i <- VUM.unsafeRead pref masked
        VUM.unsafeWrite pref masked $ i + 1
        VUM.unsafeWrite res i x
      return res
{-# INLINE radixSort64 #-}

radixSort :: (VU.Unbox a, Word64Encode a) => VU.Vector a -> VU.Vector a
radixSort = VU.map decode64 . radixSort64 . VU.map encode64
{-# INLINE radixSort #-}

radixSortInt :: VU.Vector Int -> VU.Vector Int
radixSortInt = unsafeCoerce . radixSort64 . unsafeCoerce

radixSortNonNegative :: (VU.Unbox a, Word64Encode a) => VU.Vector a -> VU.Vector a
radixSortNonNegative = VU.map decodeNonNegative64 . radixSort64 . VU.map encodeNonNegative64
{-# INLINE radixSortNonNegative #-}

radixSort32 :: VU.Vector Word32 -> VU.Vector Word32
radixSort32 vec = F.foldl' step vec ([0, 16] :: [Int])
  where
    mask k x = fromIntegral $ x .>>. k .&. 0xffff
    step v k = VU.create $ do
      pref <- VU.unsafeThaw
            . VU.prescanl' (+) 0
            . VU.unsafeAccumulate (+) (VU.replicate 0x10000 0)
            $ VU.map ((, 1) . mask k) v
      res <- VUM.unsafeNew $ VU.length v
      VU.forM_ v $ \x -> do
        let !masked = mask k x
        i <- VUM.unsafeRead pref masked
        VUM.unsafeWrite pref masked $ i + 1
        VUM.unsafeWrite res i x
      return res
{-# INLINE radixSort32 #-}

compress :: VU.Vector Int -> VU.Vector Int
compress vec = VU.create $ do
  mvec <- VUM.unsafeNew (VU.length vec)
  VU.mapM_ (\(i, x) -> VUM.unsafeWrite mvec (x .&. 0xffffffff) i) . VU.postscanl' (\(!i, !x) y ->
        if x .>>. 32 == y .>>. 32
          then (i, y)
          else (i + 1, y)
      ) (-1, -1)
    . radixSortInt
    $ VU.imap (\i x -> x .<<. 32 .|. i) vec
  return mvec
{-# INLINE compress #-}

infixl 8 .<<., .>>.

(.<<.) :: Bits b => b -> Int -> b
(.<<.) = unsafeShiftL
{-# INLINE (.<<.) #-}

(.>>.) :: Bits b => b -> Int -> b
(.>>.) = unsafeShiftR
{-# INLINE (.>>.) #-}

class Word64Encode a where
  encode64 :: a -> Word64
  decode64 :: Word64 -> a
  encodeNonNegative64 :: a -> Word64
  encodeNonNegative64 = encode64
  decodeNonNegative64 :: Word64 -> a
  decodeNonNegative64 = decode64

instance Word64Encode Int where
  encode64 x = unsafeCoerce $ x + 0x3fffffffffffffff
  decode64 x = unsafeCoerce x - 0x3fffffffffffffff
  encodeNonNegative64 = unsafeCoerce
  decodeNonNegative64 = unsafeCoerce

instance Word64Encode (Int, Int) where
  encode64 (x, y) = unsafeCoerce
    $ (x + 0x3fffffff) .<<. 31 .|. (y + 0x3fffffff)
  decode64 xy = unsafeCoerce (x, y)
    where
      !x = xy .>>. 31 - 0x3fffffff
      !y = (xy .&. 0x7fffffff) - 0x3fffffff
  encodeNonNegative64 (x, y) = unsafeCoerce $ x .<<. 31 .|. y
  decodeNonNegative64 xy     = unsafeCoerce (x, y)
    where
      !x = xy .>>. 31
      !y = xy .&. 0x7fffffff

instance Word64Encode (Int, Int, Int) where
  encode64 (x, y, z) = unsafeCoerce $ ((x + 0xfffff) .<<. 21 .|. (y + 0xfffff)) .<<. 21 .|. (z + 0xfffff)
  decode64 xyz = unsafeCoerce (x, y, z)
    where
      !x = xyz .>>. 42 - 0xfffff
      !y = (xyz .>>. 21 .&. 0x1fffff) - 0xfffff
      !z = xyz .&. 0x1fffff - 0xfffff
  encodeNonNegative64 (x, y, z) = unsafeCoerce $ (x .<<. 21 .|. y) .<<. 21 .|. z
  decodeNonNegative64 xyz = unsafeCoerce (x, y, z)
    where
      !x = xyz .>>. 42
      !y = xyz .>>. 21 .&. 0x1fffff
      !z = xyz .&. 0x1fffff