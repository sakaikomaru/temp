{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}

module FibonacciMod where

import           Data.Bits
import           GHC.Exts

plusMod :: Int -> Int -> Int -> Int
plusMod (I# x#) (I# y#) (I# m#) = case x# +# y# of
  r# -> I# (r# -# ((r# >=# m#) *# m#))
{-# INLINE plusMod #-}

minusMod :: Int -> Int -> Int -> Int
minusMod (I# x#) (I# y#) (I# m#) = case x# -# y# of
  r# -> I# (r# +# ((r# <# 0#) *# m#))
{-# INLINE minusMod #-}

mulMod :: Int -> Int -> Int -> Int
mulMod (I# x#) (I# y#) (I# mo#) = case timesWord# (int2Word# x#) (int2Word# y#) of
  z# -> case timesWord2# z# im# of
    (# q#, _ #) -> case minusWord# z# (timesWord# q# m#) of
      v#  | isTrue# (geWord# v# m#) -> I# (word2Int# (plusWord# v# m#))
          | otherwise -> I# (word2Int# v#)
  where
    m#  = int2Word# mo#
    im# = plusWord# (quotWord# 0xffffffffffffffff## m#) 1##
{-# INLINE mulMod #-}

fastDoubling :: Int -> Int -> (Int, Int)
fastDoubling 0 _ = (0, 1)
fastDoubling 1 _ = (1, 1)
fastDoubling i m
  = let
      (a, b) = fastDoubling (i .>>. 1) m
      p = mulMod a a m
      q = mulMod b b m
      r = mulMod 2 a m
      s = mulMod 2 b m
      x = minusMod s a m
      y = plusMod  r b m
    in
      if even i
        then (mulMod a x m, plusMod p q m)
        else (plusMod p q m, mulMod b y m)

-- 1 2 3 4 5 6 7 ...
-- 1 1 2 3 5 8 13...
fibMod :: Int -> Int -> Int
fibMod i m = case fastDoubling i m of (a, _) -> a

infixl 8 .>>.

(.>>.) :: Bits b => b -> Int -> b
(.>>.) = unsafeShiftR
{-# INLINE (.>>.) #-}

