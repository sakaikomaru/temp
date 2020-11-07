{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE UnboxedTuples              #-}

module Mint where

import           Data.Bits
import           Data.Coerce
import qualified Data.Ratio                  as R
import qualified Data.Vector.Generic         as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import           GHC.Exts

#define MOD 998244353

modulus :: Num a => a
modulus = MOD
{-# INLINE modulus #-}

infixr 8 ^%
infixl 7 *%, /%
infixl 6 +%, -%

(+%) :: Int -> Int -> Int
(I# x#) +% (I# y#) = case x# +# y# of
  r# -> I# (r# -# ((r# >=# MOD#) *# MOD#))
{-# INLINE (+%) #-}
(-%) :: Int -> Int -> Int
(I# x#) -% (I# y#) = case x# -# y# of
  r# -> I# (r# +# ((r# <# 0#) *# MOD#))
{-# INLINE (-%) #-}
(*%) :: Int -> Int -> Int
(I# x#) *% (I# y#) = case timesWord# (int2Word# x#) (int2Word# y#) of
  z# -> case timesWord2# z# im# of
    (# q#, _ #) -> case minusWord# z# (timesWord# q# m#) of
      v#  | isTrue# (geWord# v# m#) -> I# (word2Int# (plusWord# v# m#))
          | otherwise -> I# (word2Int# v#)
  where
    m#  = int2Word# MOD#
    im# = plusWord# (quotWord# 0xffffffffffffffff## m#) 1##
{-# INLINE (*%) #-}
(/%) :: Int -> Int -> Int
(I# x#) /% (I# y#) = go# y# MOD# 1# 0#
  where
    go# a# b# u# v#
      | isTrue# (b# ># 0#) = case a# `quotInt#` b# of
        q# -> go# b# (a# -# (q# *# b#)) v# (u# -# (q# *# v#))
      | otherwise = I# ((x# *# (u# +# MOD#)) `remInt#` MOD#)
{-# INLINE (/%) #-}
(^%) :: Int -> Int -> Int
x ^% n
  | n > 0  = go 1 x n
  | n == 0 = 1
  | otherwise = go 1 (1 /% x) (-n)
  where
    go !acc !y !m
      | m .&. 1 == 0 = go acc (y *% y) (unsafeShiftR m 1)
      | m == 1       = acc *% y
      | otherwise    = go (acc *% y) (y *% y) (unsafeShiftR (m - 1) 1)

newtype Mint = Mint { getMint :: Int }
  deriving newtype (Eq, Ord, Read, Show, Real)

mint :: Integral a => a -> Mint
mint x = fromIntegral $ mod (fromIntegral x) MOD
{-# INLINE mint #-}

mintValidate :: Mint -> Bool
mintValidate (Mint x) = 0 <= x && x < MOD
{-# INLINE mintValidate #-}

instance Bounded Mint where
  minBound = Mint 0
  maxBound = Mint $ modulus - 1

instance Enum Mint where
  toEnum = mint
  fromEnum = coerce

instance Integral Mint where
  quotRem x y = (x / y, x - x / y * y)
  toInteger = coerce (toInteger @Int)

instance Num Mint where
  (+) = coerce (+%)
  (-) = coerce (-%)
  (*) = coerce (*%)
  abs = id
  signum = const (Mint 1)
  fromInteger x = coerce @Int @Mint . fromInteger $ mod x modulus

instance Fractional Mint where
  (/) = coerce (/%)
  fromRational q = fromInteger (R.numerator q) / fromInteger (R.denominator q)

newtype instance VUM.MVector s Mint = MV_Mint (VUM.MVector s Int)
newtype instance VU.Vector Mint = V_Mint (VU.Vector Int)

instance VU.Unbox Mint

instance VGM.MVector VUM.MVector Mint where
  basicLength (MV_Mint v) = VGM.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (MV_Mint v) = MV_Mint $ VGM.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_Mint v1) (MV_Mint v2) = VGM.basicOverlaps v1 v2
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_Mint `fmap` VGM.basicUnsafeNew n
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_Mint v) = VGM.basicInitialize v
  {-# INLINE basicInitialize #-}
  basicUnsafeReplicate n x = MV_Mint `fmap` VGM.basicUnsafeReplicate n (coerce x)
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeRead (MV_Mint v) i = coerce `fmap` VGM.basicUnsafeRead v i
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_Mint v) i x = VGM.basicUnsafeWrite v i (coerce x)
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV_Mint v) = VGM.basicClear v
  {-# INLINE basicClear #-}
  basicSet (MV_Mint v) x = VGM.basicSet v (coerce x)
  {-# INLINE basicSet #-}
  basicUnsafeCopy (MV_Mint v1) (MV_Mint v2) = VGM.basicUnsafeCopy v1 v2
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV_Mint v1) (MV_Mint v2) = VGM.basicUnsafeMove v1 v2
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeGrow (MV_Mint v) n = MV_Mint `fmap` VGM.basicUnsafeGrow v n
  {-# INLINE basicUnsafeGrow #-}

instance VG.Vector VU.Vector Mint where
  basicUnsafeFreeze (MV_Mint v) = V_Mint `fmap` VG.basicUnsafeFreeze v
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_Mint v) = MV_Mint `fmap` VG.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_Mint v) = VG.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (V_Mint v) = V_Mint $ VG.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_Mint v) i = coerce `fmap` VG.basicUnsafeIndexM v i
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_Mint mv) (V_Mint v) = VG.basicUnsafeCopy mv v
  elemseq _ = seq
  {-# INLINE elemseq #-}