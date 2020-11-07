{-# LANGUAGE BangPatterns #-}

module MillerRabin where

import           Data.Bits 
import qualified GHC.Integer.GMP.Internals as GMP

millerRabin :: Int -> Bool
millerRabin k
  | k <= 3 = k == 2 || k == 3
  | even k = False
  | otherwise = mr k
  where
    mr :: Int -> Bool
    mr n
      | n < 2047            = loop [2]
      | n < 1373653         = loop [2,3]
      | n < 9080191         = loop [31,73]
      | n < 25326001        = loop [2,3,5]
      | n < 4759123141      = loop [2,7,61]
      | n < 1122004669633   = loop [2,13,23,1662803]
      | n < 2152302898747   = loop [2,3,5,7,11]
      | n < 3474749660383   = loop [2,3,5,7,11,13]
      | n < 341550071728321 = loop [2,3,5,7,11,13,17]
      | otherwise           = loop [2,325,9375,28178,450775,9780504,1795265022]
      where
        !m = n - 1
        !s = ctz m
        !d = m .>>. s
        loop :: [Int] -> Bool
        loop [] = True
        loop (a:as)
          | powModInt a d n /= 1 && allok = False
          | otherwise = loop as
          where allok = all (\r -> (powModInt a ((1 .<<. r) * d) n) /= m) $ [0..(s - 1)]

infixl 8 .<<., .>>.

(.<<.) :: Bits b => b -> Int -> b
(.<<.) = unsafeShiftL
{-# INLINE (.<<.) #-}

(.>>.) :: Bits b => b -> Int -> b
(.>>.) = unsafeShiftR
{-# INLINE (.>>.) #-}

fi :: Int -> Integer
fi = fromIntegral
{-# INLINE fi #-}

fI :: Integer -> Int
fI = fromInteger
{-# INLINE fI #-}

ctz :: FiniteBits fb => fb -> Int
ctz = countTrailingZeros
{-# INLINE ctz #-}

powModInt :: Int -> Int -> Int -> Int
powModInt !a !n !mo = fI $ GMP.powModInteger (fi a) (fi n) (fi mo)
{-# INLINE powModInt #-}