{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE BinaryLiterals   #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE TypeApplications #-}

module MersenneTwister where

import           Control.Monad
import           Control.Monad.Cont
import           Control.Monad.ST
import           Data.Bits
import           Data.Bool
import           Data.Word
import           GHC.Exts
import           System.CPUTime
import           Unsafe.Coerce
import qualified GHC.Integer.GMP.Internals         as GMP
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM
import qualified Data.Vector.Unboxed               as VU
import qualified Data.Vector.Unboxed.Mutable       as VUM

{- https://en.wikipedia.org/wiki/Mersenne_Twister -}

_pointer :: Int
_pointer = 312
{-# INLINE _pointer #-}

_lowerMask :: Word64
_lowerMask = 0b0000000000000000000000000000000001111111111111111111111111111111
{-# INLINE _lowerMask #-}

_upperMask :: Word64
_upperMask = 0b1111111111111111111111111111111110000000000000000000000000000000
{-# INLINE _upperMask #-}

type MT19937 = VUM.IOVector Word64

newMT19937 :: Word64 -> IO MT19937
newMT19937 seed = do
  mt <- VUM.unsafeNew 313 :: IO MT19937
  VUM.unsafeWrite mt _pointer 0
  VUM.unsafeWrite mt 0 seed
  range 1 311 $ \mti -> do
    item <- VUM.unsafeRead mt (mti - 1)
    let rnd = 6364136223846793005 * (item .^. (item .>>. 62)) + unsafeCoerce @Int @Word64 mti
    VUM.unsafeWrite mt mti rnd
  return mt

newRNG :: IO MT19937
newRNG = do
  t <- (fromInteger :: Integer -> Word64) <$> getCPUTime
  newMT19937 t

shiftAndXor :: Word64 -> Word64
shiftAndXor w0 =
  case w0 .^. ((w0 .>>. 29) .&. 0x5555555555555555) of
    w1 -> case w1 .^. ((w1 .<<. 17) .&. 0x71D67FFFEDA60000) of
      w2 -> case w2 .^. ((w2 .<<. 37) .&. 0xFFF7EEE000000000) of
        w3 -> w3 .^. (w3 .>>. 43)

twist :: MT19937 -> IO ()
twist mt = do
  rep 312 $ \i -> do
    item1 <- VUM.unsafeRead mt i
    item2 <- VUM.unsafeRead mt ((i + 1) `mod` 312)
    let
      x  = (item1 .&. _upperMask) + (item2 .&. _lowerMask)
      xA = x .>>. 1
      xa  = if odd x then xA .^. 0xB5026F5AA96619E9 else xA
    item3 <- VUM.unsafeRead mt ((i + 156) `mod` 312)
    VUM.unsafeWrite mt i (item3 .^. xa)
  VUM.unsafeWrite mt _pointer 0

nextWord64 :: MT19937 -> IO Word64
nextWord64 mt = do
  idx <- VUM.unsafeRead mt _pointer
  when (idx >= 312) $ twist mt
  y <- shiftAndXor <$> VUM.unsafeRead mt (bool (fromIntegral idx) 0 (idx >= 312))
  VUM.unsafeModify mt succ _pointer
  return y

nextInt :: MT19937 -> IO Int
nextInt mt = unsafeCoerce <$> nextWord64 mt

nextWord :: MT19937 -> IO Word
nextWord mt = unsafeCoerce <$> nextWord64 mt

nextDouble :: MT19937 -> IO Double
nextDouble mt19937 = do
  t <- nextWord64 mt19937
  let x = 0x3ff .<<. 52 .|. t .>>. 12
  return $! unsafeCoerce @Word64 @Double x - 1.0

nextGauss :: MT19937 -> Double -> Double -> IO Double
nextGauss mt19937 mu sigma = do
  x <- nextDouble mt19937
  y <- nextDouble mt19937
  let z = sqrt (-2.0 * log x) * cos (2.0 * pi * y)
  return $! sigma * z + mu

randomR :: MT19937 -> Int -> Int -> IO Int
randomR mt19937 l r = (+ l) . flip mod (r - l + 1) <$> nextInt mt19937

shuffleM :: VUM.Unbox a => MT19937 -> VUM.IOVector a -> IO ()
shuffleM mt19937 mvec = do
  rev (VUM.length mvec) $ \i -> do
    j <- nextWord64 mt19937
    VUM.unsafeSwap mvec i (unsafeCoerce $ rem j (unsafeCoerce i + 1))

shuffle :: VU.Unbox a => MT19937 -> VU.Vector a -> IO (VU.Vector a)
shuffle mt19937 vec = do
  mv <- VU.unsafeThaw vec
  shuffleM mt19937 mv
  VU.unsafeFreeze mv

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