{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash    #-}

module PollardRho where

import           Control.Monad.Fix
import           Control.Monad.ST
import           Data.Bits
import           Data.Word
import           GHC.Exts
import           Unsafe.Coerce
import qualified GHC.Integer.GMP.Internals   as GMP
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

nextRandF :: Int -> Int -> Int -> Int
nextRandF x n c = (x * x + c) `mod` n

factorRho :: Int -> [Int]
factorRho n
  | n <= 1        = []
  | even n        = replicate s 2 ++ factorRho d
  | n `mod`  3 == 0 =  3 : factorRho (n `div`  3)
  | n `mod`  5 == 0 =  5 : factorRho (n `div`  5)
  | n `mod`  7 == 0 =  7 : factorRho (n `div`  7)
  | n `mod` 11 == 0 = 11 : factorRho (n `div` 11)
  | n `mod` 13 == 0 = 13 : factorRho (n `div` 13)
  | n `mod` 17 == 0 = 17 : factorRho (n `div` 17)
  | n `mod` 19 == 0 = 19 : factorRho (n `div` 19)
  | n `mod` 23 == 0 = 23 : factorRho (n `div` 23)
  | millerRabin n = [n]
  | otherwise = y : factorRho (n `div` y)
  where
    x  = pollardRho n
    y  = if millerRabin x then x else pollardRho x 
    !s = ctz n
    !d = n .>>. s

pollardRho :: Int -> Int
pollardRho k = runST $ do
  xyd <- VU.unsafeThaw $ VU.fromList ([2,2,1] :: [Int])
  flip fix 1 $ \loop !c -> do
    itemd <- VUM.unsafeRead xyd 2
    if itemd /= 1
      then return itemd
      else do
        itemx <- VUM.unsafeRead xyd 0
        itemy <- VUM.unsafeRead xyd 1
        let
          xx = nextRandF itemx k c
          yy = nextRandF (nextRandF itemy k c) k c
          dd = gcd (abs (xx - yy)) k
        VUM.unsafeWrite xyd 0 xx
        VUM.unsafeWrite xyd 1 yy
        VUM.unsafeWrite xyd 2 dd
        if dd /= k
          then loop c
          else do
            VUM.unsafeWrite xyd 0 (2 :: Int)
            VUM.unsafeWrite xyd 1 (2 :: Int)
            VUM.unsafeWrite xyd 2 (1 :: Int)
            loop (c + 2)

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
          where allok = all (\r -> (powModInt a ((1 .<<. r) * d) n) /= m) [0..(s - 1)]

powModInt :: Int -> Int -> Int -> Int
powModInt a n mo = fI $ GMP.powModInteger (fi a) (fi n) (fi mo)

recipModInt :: Int -> Int -> Int
recipModInt a mo = fI $ GMP.recipModInteger (fi a) (fi mo)

floorSqrt :: Int -> Int
floorSqrt = floor . sqrt . fromIntegral

floorLog2 :: Int -> Int
floorLog2 x = fromIntegral $ y .>>. 52 - 1023
  where
    y :: Word64
    y = unsafeCoerce (fromIntegral x :: Double)

clz :: FiniteBits fb => fb -> Int
clz = countLeadingZeros
{-# INLINE clz #-}

ctz :: FiniteBits fb => fb -> Int
ctz = countTrailingZeros
{-# INLINE ctz #-}

ceilPow2 :: Int -> Int
ceilPow2 n
  | n > 1     = (-1) .>>>. (clz (n - 1)) + 1
  | otherwise = 1

floorPow2 :: Int -> Int
floorPow2 n
  | n >= 1    = 1 .<<. (63 - (clz n))
  | otherwise = 0

fi :: Int -> Integer
fi = fromIntegral
{-# INLINE fi #-}

fI :: Integer -> Int
fI = fromInteger
{-# INLINE fI #-}

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
(.^.) = xor
{-# INLINE (.^.) #-}