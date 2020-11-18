{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash    #-}

module ZetaMoebius where

import           Control.Monad.Cont
import           Control.Monad.ST
import           Data.Bits
import           GHC.Exts

import qualified Data.Vector.Fusion.Stream.Monadic as VFSM
import qualified Data.Vector.Unboxed               as VU
import qualified Data.Vector.Unboxed.Mutable       as VUM

fztAND, fmtAND, fztOR, fmtOR, fztXOR, fmtXOR, fztDIV, fmtDIV :: VU.Vector Int -> VU.Vector Int
fztAND vec = VU.create $ do
  let
    !f = growVU vec
    !n = VU.length f
  g <- VU.unsafeThaw f
  forG 1 (n - 1) const 0 unsafeShiftL 1 $ \i -> rep n $ \j -> when (j .&. i == 0) $ do
    item <- VUM.unsafeRead g (j .|. i)
    VUM.unsafeModify g (+ item) j
  return g

fmtAND vec = VU.create $ do
  let
    !f = growVU vec
    !n = VU.length f
  g <- VU.unsafeThaw f
  forG 1 (n - 1) const 0 unsafeShiftL 1 $ \i -> rep n $ \j -> when (j .&. i == 0) $ do
    item <- VUM.unsafeRead g (j .|. i)
    VUM.unsafeModify g (subtract item) j
  return g

fztOR vec = VU.create $ do
  let
    !f = growVU vec
    !n = VU.length f
  g <- VU.unsafeThaw f
  forG 1 (n - 1) const 0 unsafeShiftL 1 $ \i -> rep n $ \j -> when (j .&. i == 0) $ do
    item <- VUM.unsafeRead g j
    VUM.unsafeModify g (+ item) (j .|. i)
  return g

fmtOR vec = VU.create $ do
  let
    !f = growVU vec
    !n = VU.length f
  g <- VU.unsafeThaw f
  forG 1 (n - 1) const 0 unsafeShiftL 1 $ \i -> rep n $ \j -> when (j .&. i == 0) $ do
    item <- VUM.unsafeRead g j
    VUM.unsafeModify g (subtract item) (j .|. i)
  return g

fztXOR vec = VU.create $ do
  let
    !f = growVU vec
    !n = VU.length f
  g <- VU.unsafeThaw f
  forG 1 (n - 1) const 0 unsafeShiftL 1 $ \i -> rep n $ \j -> when (j .&. i == 0) $ do
    itemX <- VUM.unsafeRead g j
    itemY <- VUM.unsafeRead g (j .|. i)
    VUM.unsafeWrite g j (itemX + itemY)
    VUM.unsafeWrite g (j .|. i) (itemX - itemY)
  return g

fmtXOR vec = VU.create $ do
  let
    !f = growVU vec
    !n = VU.length f
  g <- VU.unsafeThaw f
  forG 1 (n - 1) const 0 unsafeShiftL 1 $ \i -> rep n $ \j -> when (j .&. i == 0) $ do
    itemX <- VUM.unsafeRead g j
    itemY <- VUM.unsafeRead g (j .|. i)
    VUM.unsafeWrite g j ((itemX + itemY) `div` 2)
    VUM.unsafeWrite g (j .|. i) ((itemX - itemY) `div` 2)
  return g

fztDIV vec = VU.create $ do
  let !n = VU.length vec
  sieve <- VUM.replicate n True
  g <- VU.unsafeThaw vec
  forG 2 (n - 1) const 0 (+) 1 $ \p -> do
    b <- VUM.unsafeRead sieve p
    when b $ forG 1 (n - 1) (*) p (+) 1 $ \k -> do
      VUM.unsafeWrite sieve (k * p) False
      VUM.unsafeModify g (+(vec VU.! k)) (k * p)
  return g

fmtDIV vec = VU.create $ do
  let !n = VU.length vec
  sieve <- VUM.replicate n True
  g <- VU.unsafeThaw vec
  forG 2 (n - 1) const 0 (+) 1 $ \p -> do
    b <- VUM.unsafeRead sieve p
    when b $ forRG ((n - 1) `div` p) 1 const 0 (-) 1 $ \k -> do
      VUM.unsafeWrite sieve (k * p) False
      VUM.unsafeModify g (subtract (vec VU.! k)) (k * p)
  return g

growVU :: VU.Vector Int -> VU.Vector Int
growVU v
  | VU.null v = VU.singleton 0
  | VU.length v == 1 = v
  | otherwise = v VU.++ VU.replicate (ceilPow2 n - n) 0
  where
    !n = VU.length v

ctz :: FiniteBits fb => fb -> Int
ctz = countTrailingZeros
{-# INLINE ctz #-}

clz :: FiniteBits fb => fb -> Int
clz = countLeadingZeros
{-# INLINE clz #-}

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

ceilPow2 :: Int -> Int
ceilPow2 n
  | n > 1     = (-1) .>>>. clz (n - 1) + 1
  | otherwise = 1
{-# INLINE ceilPow2 #-}

-- | for (int i = l; f(i, p) <= r ; g(i, d))
streamG :: Monad m => Int -> Int -> (Int -> Int -> Int) -> Int -> (Int -> Int -> Int) -> Int -> VFSM.Stream m Int
streamG l r f p g d = VFSM.Stream step l
  where
    step x
      | f x p <= r = return $ VFSM.Yield x (g x d)
      | otherwise  = return VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] streamG #-}

forG :: Monad m => Int -> Int -> (Int -> Int -> Int) -> Int -> (Int -> Int -> Int) -> Int -> (Int -> m ()) -> m ()
forG l r f p g d = flip VFSM.mapM_ (streamG l r f p g d)
{-# INLINE forG #-}

-- | for (int i = r; f(i, p) >= l; g(i, d))
streamRG :: Monad m => Int -> Int -> (Int -> Int -> Int) -> Int -> (Int -> Int -> Int) -> Int -> VFSM.Stream m Int
streamRG r l f p g d = VFSM.Stream step r
  where
    step x
      | f x p >= l = return $ VFSM.Yield x (g x d)
      | otherwise  = return VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] streamRG #-}

forRG :: Monad m => Int -> Int -> (Int -> Int -> Int) -> Int -> (Int -> Int -> Int) -> Int -> (Int -> m ()) -> m ()
forRG r l f p g d = flip VFSM.mapM_ (streamRG r l f p g d)
{-# INLINE forRG #-}

withBreakIO :: ((r -> ContT r IO b) -> ContT r IO r) -> IO r
withBreakIO = flip runContT pure . callCC
{-# INLINE withBreakIO #-}

withBreakST :: ((r -> ContT r (ST s) b) -> ContT r (ST s) r) -> (ST s) r
withBreakST = flip runContT pure . callCC
{-# INLINE withBreakST #-}

-- | for (int i = 0; i < n; i++)
rep :: Monad m => Int -> (Int -> m ()) -> m ()
rep n = flip VFSM.mapM_ (streamG 0 (n - 1) const 0 (+) 1)


{- fztAND
S = { 3, 1, 4, 1, 5, 9, 2, 6 }               fzt S
0b11111111 -> 3 + 1 + 4 + 1 + 5 + 9 + 2 + 6 =  31
0b01010101 ->     1   +   1   +   9   +   6 =  17
0b00110011 ->         4 + 1     +     2 + 6 =  13
0b00010001 ->             1       +       6 =   7
0b00001111 ->                 5 + 9 + 2 + 6 =  22
0b00000101 ->                     9   +   6 =  15
0b00000011 ->                         2 + 6 =   8
0b00000001 ->                             6 =   6
-}
{- fmtAND
T = { 31, 17, 13, 7, 22, 15, 8, 6}             S
0b11111111 -> 31 - 1 - 4 - 1 - 5 - 9 - 2 - 6 = 3
0b01010101 -> 17 - 0 - 1 - 0 - 9 - 0 - 6 - 0 = 1
0b00110011 -> 13 - 1 - 0 - 0 - 2 - 6 - 0 - 0 = 4
0b00010001 ->  7 - 0 - 0 - 0 - 6 - 0 - 0 - 0 = 1
0b00001111 -> 22 - 9 - 2 - 6 - 0 - 0 - 0 - 0 = 5
0b00000101 -> 15 - 0 - 6 - 0 - 0 - 0 - 0 - 0 = 9
0b00000011 ->  8 - 6 - 0 - 0 - 0 - 0 - 0 - 0 = 2
0b00000001 ->  6 - 0 - 0 - 0 - 0 - 0 - 0 - 0 = 6
-}
{- fztOR
S = { 3, 1, 4, 1, 5, 9, 2, 6 }               fzt S
0b10000000 -> 3 + 0 + 0 + 0 + 0 + 0 + 0 + 0 =  3
0b11000000 -> 3 + 1 + 0 + 0 + 0 + 0 + 0 + 0 =  4
0b10100000 -> 3 + 0 + 4 + 0 + 0 + 0 + 0 + 0 =  7
0b11110000 -> 3 + 1 + 4 + 1 + 0 + 0 + 0 + 0 =  9
0b10001000 -> 3 + 0 + 0 + 0 + 5 + 0 + 0 + 0 =  8
0b11001100 -> 3 + 1 + 0 + 0 + 5 + 9 + 0 + 0 = 18
0b10101010 -> 3 + 0 + 4 + 0 + 5 + 0 + 2 + 0 = 14
0b11111111 -> 3 + 1 + 4 + 1 + 5 + 9 + 2 + 6 = 31
-}
{- fztXOR
S = { 3, 1, 4, 1, 5, 9, 2, 6 }
0b11111111 -> + 3 + 1 + 4 + 1 + 5 + 9 + 2 + 6 = 31
0b10101010 -> + 3 - 1 + 4 - 1 + 5 - 9 + 2 - 6 = -3
0b11001100 -> + 3 + 1 - 4 - 1 + 5 + 9 - 2 - 6 =  5
0b10011001 -> + 3 - 1 - 4 + 1 + 5 - 9 - 2 + 6 = -1
0b11110000 -> + 3 + 1 + 4 + 1 - 5 - 9 - 2 - 6 = -13
0b10100101 -> + 3 - 1 + 4 - 1 - 5 + 9 - 2 + 6 = 13
0b11000011 -> + 3 + 1 - 4 - 1 - 5 - 9 + 2 + 6 = -7
0b10010110 -> + 3 - 1 - 4 + 1 - 5 + 9 + 2 - 6 = -1
-}
{- fztDIV
idx = {  0, 1, 2, 3, 4, 5, 6, 7, 8  }
S   = {  0, 1, 1, 1, 1, 1, 1, 1, 1  }
T   = {  0, 1, 2, 2, 3, 2, 4, 2, 4  }
idx = {  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20 }
S   = {  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1 }
T   = {  3,  4,  2,  6,  2,  4,  4,  5,  2,  6,  2,  6 }
素数p = { 1, p } = 1のスコア + pのスコア
4 = { 1, 2, 4 } = 1のスコア + 2のスコア + 4のスコア
12 = { 1, 2, 3, 4, 6, 12 }
20 = { 1, 2, 4, 5, 10, 20 }
-}

