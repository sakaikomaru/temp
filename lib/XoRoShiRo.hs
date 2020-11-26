{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}

module XoRoShiRo where

import           Control.Monad.Cont
import           Control.Monad.ST
import           Data.Bits
import           Data.Word
import           GHC.Exts
import           Unsafe.Coerce
import           System.CPUTime
import qualified GHC.Integer.GMP.Internals         as GMP
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM
import qualified Data.Vector.Unboxed               as VU
import qualified Data.Vector.Unboxed.Mutable       as VUM

type RNG = VUM.IOVector Word

getSeed :: IO Word
getSeed = do
  t <- getCPUTime
  return (fromInteger t * 0x73fe981a)

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

randomR :: RNG -> Int -> Int -> IO Int
randomR rng l r = (+ l) . flip mod (r - l + 1) <$> nextInt rng

-------------------------------------------------------------------------------
-- for
-------------------------------------------------------------------------------
rep :: Monad m => Int -> (Int -> m ()) -> m ()
rep n = flip VFSM.mapM_ (streamG 0 (n - 1) const 0 (+) 1)
{-# INLINE rep #-}

rep' :: Monad m => Int -> (Int -> m ()) -> m ()
rep' n = flip VFSM.mapM_ (streamG 0 n const 0 (+) 1)
{-# INLINE rep' #-}

rep1 :: Monad m => Int -> (Int -> m ()) -> m ()
rep1 n = flip VFSM.mapM_ (streamG 1 (n - 1) const 0 (+) 1)
{-# INLINE rep1 #-}

rep1' :: Monad m => Int -> (Int -> m ()) -> m ()
rep1' n = flip VFSM.mapM_ (streamG 1 n const 0 (+) 1)
{-# INLINE rep1' #-}

rev :: Monad m => Int -> (Int -> m ()) -> m ()
rev n = flip VFSM.mapM_ (streamRG (n - 1) 0 const 0 (-) 1)
{-# INLINE rev #-}

rev' :: Monad m => Int -> (Int -> m ()) -> m ()
rev' n = flip VFSM.mapM_ (streamRG n 0 const 0 (-) 1)
{-# INLINE rev' #-}

rev1 :: Monad m => Int -> (Int -> m ()) -> m ()
rev1 n = flip VFSM.mapM_ (streamRG (n - 1) 1 const 0 (-) 1)
{-# INLINE rev1 #-}

rev1' :: Monad m => Int -> (Int -> m ()) -> m ()
rev1' n = flip VFSM.mapM_ (streamRG n 1 const 0 (-) 1)
{-# INLINE rev1' #-}

range :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
range l r = flip VFSM.mapM_ (streamG l r const 0 (+) 1)
{-# INLINE range #-}

rangeR :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
rangeR r l = flip VFSM.mapM_ (streamRG r l const 0 (-) 1)
{-# INLINE rangeR #-}

forP :: Monad m => Int -> (Int -> m ()) -> m ()
forP p = flip VFSM.mapM_ (streamG 2 p (^) 2 (+) 1)
{-# INLINE forP #-}

forG :: Monad m => Int -> Int -> (Int -> Int -> Int) -> Int -> (Int -> Int -> Int) -> Int -> (Int -> m ()) -> m ()
forG l r f p g d = flip VFSM.mapM_ (streamG l r f p g d)
{-# INLINE forG #-}

forRG :: Monad m => Int -> Int -> (Int -> Int -> Int) -> Int -> (Int -> Int -> Int) -> Int -> (Int -> m ()) -> m ()
forRG r l f p g d = flip VFSM.mapM_ (streamRG r l f p g d)
{-# INLINE forRG #-}

streamG :: Monad m => Int -> Int -> (Int -> Int -> Int) -> Int -> (Int -> Int -> Int) -> Int -> VFSM.Stream m Int
streamG !l !r !f !p !g !d = VFSM.Stream step l
  where
    step x
      | f x p <= r = return $ VFSM.Yield x (g x d)
      | otherwise  = return VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] streamG #-}

streamRG :: Monad m => Int -> Int -> (Int -> Int -> Int) -> Int -> (Int -> Int -> Int) -> Int -> VFSM.Stream m Int
streamRG !r !l !f !p !g !d = VFSM.Stream step r
  where
    step x
      | f x p >= l = return $ VFSM.Yield x (g x d)
      | otherwise  = return VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] streamRG #-}

withBreakIO :: ((r -> ContT r IO b) -> ContT r IO r) -> IO r
withBreakIO = flip runContT pure . callCC
{-# INLINE withBreakIO #-}

withBreakST :: ((r -> ContT r (ST s) b) -> ContT r (ST s) r) -> (ST s) r
withBreakST = flip runContT pure . callCC
{-# INLINE withBreakST #-}

-------------------------------------------------------------------------------
-- util
-------------------------------------------------------------------------------
fi :: Int -> Integer
fi = fromIntegral
{-# INLINE fi #-}

fI :: Integer -> Int
fI = fromInteger
{-# INLINE fI #-}

powModInt :: Int -> Int -> Int -> Int
powModInt a b c = fI $ GMP.powModInteger (fi a) (fi b) (fi c)
{-# INLINE powModInt #-}

recipModInt :: Int -> Int -> Int
recipModInt a m = fI $ GMP.recipModInteger (fi a) (fi m)
{-# INLINE recipModInt #-}

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
(.^.)  = xor
{-# INLINE (.^.)  #-}

clz :: FiniteBits fb => fb -> Int
clz = countLeadingZeros
{-# INLINE clz #-}

ctz :: FiniteBits fb => fb -> Int
ctz = countTrailingZeros
{-# INLINE ctz #-}

encode32x2 :: Int -> Int -> Int
encode32x2 x y = x .<<. 32 .|. y
{-# INLINE encode32x2 #-}

decode32x2 :: Int -> (Int, Int)
decode32x2 xy =
    let !x = xy .>>>. 32
        !y = xy .&. 0xffffffff
    in (x, y)
{-# INLINE decode32x2 #-}

ceilPow2 :: Int -> Int
ceilPow2 n
  | n > 1     = (-1) .>>>. clz (n - 1) + 1
  | otherwise = 1
{-# INLINE ceilPow2 #-}

floorPow2 :: Int -> Int
floorPow2 n
  | n >= 1    = 1 .<<. (63 - clz n)
  | otherwise = 0
{-# INLINE floorPow2 #-}

bitReverse :: Int -> Int
bitReverse
  = unsafeCoerce
  . step 32 0xffffffff00000000 0x00000000ffffffff
  . step 16 0xffff0000ffff0000 0x0000ffff0000ffff
  . step 08 0xff00ff00ff00ff00 0x00ff00ff00ff00ff
  . step 04 0xf0f0f0f0f0f0f0f0 0x0f0f0f0f0f0f0f0f
  . step 02 0xcccccccccccccccc 0x3333333333333333
  . step 01 0xaaaaaaaaaaaaaaaa 0x5555555555555555
  . unsafeCoerce
  where
    step :: Int -> Word64 -> Word64 -> Word64 -> Word64
    step i ml mr = \ !x -> (x .&. ml) .>>. i .|. (x .&. mr) .<<. i
    {-# INLINE step #-}