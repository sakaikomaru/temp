{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Lattice where

import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.ST
import           Data.Bits
import           Data.Coerce
import qualified Data.List                         as L
import           Data.Word
import           GHC.Exts
import           Unsafe.Coerce
import qualified GHC.Integer.GMP.Internals         as GMP
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM
import qualified Data.Vector.Unboxed               as VU
import qualified Data.Vector.Unboxed.Mutable       as VUM

infix 4 .<.
class Poset a where
  (.<.) :: a -> a -> Bool
  zeta :: (Integral i) => a -> a -> i
  zeta x y
    | x .<. y = 1
    | otherwise = 0
  moebius :: (Integral i) => a -> a -> i

class Lattice a where
    (/\) :: a -> a -> a
    (\/) :: a -> a -> a

class FastZetaMoebius f where
  type Dim f
  fastZeta :: (Num a, VU.Unbox a) => (Int -> f Int) -> Dim f -> VUM.IOVector a -> IO ()
  fastMoebius :: (Num a, VU.Unbox a) => (Int -> f Int) -> Dim f -> VUM.IOVector a -> IO ()

newtype NatOrd a = NatOrd { getNatOrd :: a }
  deriving (Eq, Ord, Show)

instance (Integral a, Ord a) => Poset (NatOrd a) where
  (.<.) = (<=)
  {-# INLINE (.<.) #-}
  moebius (NatOrd x) (NatOrd y)
    | x == y     = 1
    | x + 1 == y = -1
    | otherwise  = 0

instance Ord a => Lattice (NatOrd a) where
  (/\) = coerce (min @a)
  {-# INLINE (/\) #-}
  (\/) = coerce (max @a)
  {-# INLINE (\/) #-}

newtype DivOrd a = DivOrd { getDivOrd :: a }
  deriving (Eq, Show)

instance Integral a => Poset (DivOrd a) where
  (.<.)  (DivOrd x) (DivOrd y) = rem y x == 0
  {-# INLINE (.<.) #-}
  moebius (DivOrd x) (DivOrd y)
    | not $ DivOrd x .<. DivOrd y = 0
    | otherwise = product . map mu . L.group $ primeFactors (quot y x) -- factorRho (fromIntegral $ quot y x)
    where
      mu [_] = -1
      mu _   = 0

instance Integral a => Lattice (DivOrd a) where
  (/\) = coerce (gcd @a)
  {-# INLINE (/\) #-}
  (\/) = coerce (lcm @a)
  {-# INLINE (\/) #-}

instance FastZetaMoebius DivOrd where
  type Dim DivOrd = VU.Vector Int
  fastZeta _ primes g = do
    let n = VUM.length g
    when (n > 0) $ do
      g0 <- VUM.read g 0
      VU.forM_ primes $ \p -> rev (quot (n - 1) p + 1) $ \i -> do
        c <- VUM.unsafeRead g (p * i)
        VUM.unsafeModify g (+ c) i
      VUM.write g 0 g0
  {-# INLINE fastZeta #-}
  fastMoebius _ primes f = do
    let n = VUM.length f
    when (n > 0) $ do
      f0 <- VUM.read f 0
      VU.forM_ primes $ \p -> rep (quot (n - 1) p + 1) $ \i -> do
        c <- VUM.unsafeRead f (p * i)
        VUM.unsafeModify f (subtract c) i
      VUM.write f 0 f0
  {-# INLINE fastMoebius #-}

newtype SetOrd a = SetOrd { getSetOrd :: a }
  deriving (Eq, Show)

instance Bits a => Poset (SetOrd a) where
  (.<.) (SetOrd x) (SetOrd y) = x .&. y == x
  {-# INLINE (.<.) #-}
  moebius (SetOrd x) (SetOrd y)
    | not $ SetOrd x .<. SetOrd y = 0
    | testBit (popCount $ complement x .&. y) 0 = -1
    | otherwise = 1

instance Bits a => Lattice (SetOrd a) where
  (/\) = coerce ((.&.) @a)
  {-# INLINE (/\) #-}
  (\/) = coerce ((.|.) @a)
  {-# INLINE (\/) #-}

instance FastZetaMoebius SetOrd where
  type Dim SetOrd = Int
  fastZeta _ n g = do
    rep n $ \i -> rep (unsafeShiftL 1 n) $ \j -> do
      unless (testBit j i) $ do
        c <- VUM.unsafeRead g (setBit j i)
        VUM.unsafeModify g (+ c) j
  {-# INLINE fastZeta #-}
  fastMoebius _ n f = do
    rep n $ \i -> rep (unsafeShiftL 1 n) $ \j -> do
      unless (testBit j i) $ do
        c <- VUM.unsafeRead f (setBit j i)
        VUM.unsafeModify f (subtract c) j
  {-# INLINE fastMoebius #-}

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
          where allok = all (\r -> powModInt a ((1 .<<. r) * d) n /= m) [0..(s - 1)]

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
  | n > 1     = (-1) .>>>. clz (n - 1) + 1
  | otherwise = 1

floorPow2 :: Int -> Int
floorPow2 n
  | n >= 1    = 1 .<<. (63 - clz n)
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

stream :: Monad m => Int -> Int -> VFSM.Stream m Int
stream !l !r = VFSM.Stream step l
  where
    step x
      | x < r     = return $ VFSM.Yield x (x + 1)
      | otherwise = return VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] stream #-}

rep :: Monad m => Int -> (Int -> m ()) -> m ()
rep n = flip VFSM.mapM_ (stream 0 n)
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

smallPrimes :: Integral int => [int]
smallPrimes = 2 : [n | n <- [3, 5 .. 46337], all ((> 0) . rem n) $ takeWhile (\x -> x * x <= n) smallPrimes]
{-# SPECIALIZE smallPrimes :: [Int] #-}

primeFactors :: Integral int => int -> [int]
primeFactors n | n < 2 = []
primeFactors n = go n smallPrimes
  where
    go !n pps@(p:ps)
      | n < p * p = [n]
      | r > 0     = go n ps
      | otherwise = p : go q pps
      where
        (q, r) = quotRem n p
    go n [] = [n]
{-# SPECIALIZE primeFactors :: Int -> [Int] #-}