{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE TypeApplications #-}

module SqrtMod where

import           Data.Bits
import           Unsafe.Coerce
import qualified GHC.Integer.GMP.Internals         as GMP
import           Data.Time.Clock.POSIX             (getPOSIXTime)
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM
import qualified Data.Vector.Unboxed               as VU
import qualified Data.Vector.Unboxed.Mutable       as VUM

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


type RNG = VUM.IOVector Word

getSeed :: IO Word
getSeed = do
  t <- getPOSIXTime
  let ret = floor $ t * 1e9 :: Word
  if ret == 0
    then getSeed
    else return ret

splitMix64 :: Word -> (Word, Word)
splitMix64 state = case state + 0x9e3779b97f4a7c15 of
  z0 -> case (z0 .^. z0 .>>. 30) * 0xbf58476d1ce4e5b9 of
    z1 -> case (z1 .^. z1 .>>. 27) * 0x94d049bb133111eb of
      z2 -> case z2 .^. z2 .>>. 31 of
        z3 -> (z0, z3)

newRNG :: IO RNG
newRNG = newRNGWithSeed =<< getSeed

newRNGWithSeed :: Word -> IO RNG
newRNGWithSeed seed = do
  let (s0, seed') = splitMix64 seed
  let (s1, _)     = splitMix64 seed'
  rng <- VUM.unsafeNew 2
  VUM.unsafeWrite rng 0 s0
  VUM.unsafeWrite rng 1 s1
  return rng

nextWord :: RNG -> IO Word
nextWord rng = do
  !s0 <-            VUM.unsafeRead rng 0
  !s1 <- xor s0 <$> VUM.unsafeRead rng 1
  VUM.unsafeWrite rng 0 $ (s0 .<<. 24  .|.  s0 .>>. 40) .^. s1 .^. (s1 .<<. 16)
  VUM.unsafeWrite rng 1 $ (s1 .<<. 37) .|. (s1 .>>. 27)
  let s05 = s0 * 5
  return $! (s05 .<<. 7 .|. s05 .>>. 57) * 9

nextInt :: RNG -> IO Int
nextInt rng = unsafeCoerce @Word @Int <$> nextWord rng

nextDouble :: RNG -> IO Double
nextDouble rng = do
  t <- nextWord rng
  let x = 0x3ff .<<. 52 .|. t .>>. 12
  return $! unsafeCoerce @Word @Double x - 1.0

nextGauss :: RNG -> Double -> Double -> IO Double
nextGauss rng mu sigma = do
  x <- nextDouble rng
  y <- nextDouble rng
  let z = sqrt (-2.0 * log x) * cos (2.0 * pi * y)
  return $! sigma * z + mu

withRNG :: (RNG -> IO a) -> IO a
withRNG f = do
  seed <- getSeed
  withRNGWithSeed seed f

withRNGWithSeed :: Word -> (RNG -> IO a) -> IO a
withRNGWithSeed seed f = newRNGWithSeed seed >>= f

shuffleM :: VUM.Unbox a => RNG -> VUM.IOVector a -> IO ()
shuffleM rng mvec = do
  rev (VUM.length mvec) $ \i -> do
    j <- nextWord rng
    VUM.unsafeSwap mvec i (unsafeCoerce $ rem j (unsafeCoerce i + 1))

shuffle :: VU.Unbox a => VU.Vector a -> IO (VU.Vector a)
shuffle vec = withRNG $ \rng -> do
  mv <- VU.unsafeThaw vec
  shuffleM rng mv
  VU.unsafeFreeze mv

randomR :: RNG -> (Int, Int) -> IO Int
randomR rng (l, r) = flip mod (r - l + 1) <$> nextInt rng

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