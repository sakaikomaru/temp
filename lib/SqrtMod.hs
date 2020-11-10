{-# LANGUAGE BangPatterns     #-}

module SqrtMod where

import           Data.Bits
import qualified GHC.Integer.GMP.Internals         as GMP
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM

sqrtTS :: Int -> Int -> IO Int
sqrtTS a p
  | a == 0 = return 0
  | a == 1 = return 1
  | legendreSymbol a p /= 1 = return (- 1)
  | otherwise = return r
  where
    !s = ctz (p - 1)
    !q = (p - 1) .>>. s
    z  = head $ filter (\t -> legendreSymbol t p == p - 1) [1..]
    (r, _, _, _) = until (\(_, t, _, _) -> t == 1 ) iter (powModInt a ((q + 1) .>>. 1) p, powModInt a q p, s, powModInt z q p)
    iter (_r, t, m, c) = (_r * b `rem` p, t * b2 `rem` p, i, b2)
      where
        i = fst $ head $ filter (\(_, x) -> x == 1) $ zip [0 .. m - 1] $ iterate (\t0 -> t0 * t0 `rem` p) t
        b = powModInt c k p
        b2 = b * b `rem` p
        k = 1 .<<. (m - i - 1)

legendreSymbol :: Int -> Int -> Int
legendreSymbol a p = powModInt a ((p - 1) `div` 2) p

infixl 8 .>>., .<<.
infixl 6 .^.

(.>>.) :: Bits b => b -> Int -> b
(.>>.) = unsafeShiftR
{-# INLINE (.>>.) #-}

(.<<.) :: Bits b => b -> Int -> b
(.<<.) = unsafeShiftL
{-# INLINE (.<<.) #-}

(.^.) :: Bits b => b -> b -> b
(.^.) = xor
{-# INLINE (.^.) #-}

clz ::FiniteBits fb => fb -> Int
clz = countLeadingZeros
{-# INLINE clz #-}

ctz :: FiniteBits fb => fb -> Int
ctz = countTrailingZeros
{-# INLINE ctz #-}

powModInt :: Int -> Int -> Int -> Int
powModInt a b mo = fI $ GMP.powModInteger (fi a) (fi b) (fi mo)
{-# INLINE powModInt #-}

fi :: Int -> Integer
fi = fromIntegral
{-# INLINE fi #-}

fI :: Integer -> Int
fI = fromInteger
{-# INLINE fI #-}

stream :: Monad m => Int -> Int -> VFSM.Stream m Int
stream !l !r = VFSM.Stream step l
  where
    step x
      | x < r     = return $ VFSM.Yield x (x + 1)
      | otherwise = return $ VFSM.Done
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
      | otherwise = return $ VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] streamR #-}

rev :: Monad m => Int -> (Int -> m ()) -> m ()
rev n = flip VFSM.mapM_ (streamR 0 n)
{-# INLINE rev #-}