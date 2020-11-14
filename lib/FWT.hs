{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module FWT where

import Control.Monad
import Data.Bits
import GHC.Exts
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM
import qualified Data.Vector.Unboxed               as VU
import qualified Data.Vector.Unboxed.Mutable       as VUM

-- 高速ゼータ変換(上位集合)
fztU :: VU.Vector Int -> VU.Vector Int
fztU f' = VU.create $ do
  let
    !f = growVU f'
    !n = VU.length f
  g <- VU.unsafeThaw f
  rep' n $ \i -> rep n $ \j -> when (j .&. i == 0) $ do
    item <- VUM.unsafeRead g (j .|. i)
    VUM.unsafeModify g (+ item) j
  return g

-- 高速メビウス変換(上位集合)
fmtU :: VU.Vector Int -> VU.Vector Int
fmtU f' = VU.create $ do
  let
    !f = growVU f'
    !n = VU.length f
  g <- VU.unsafeThaw f
  rep' n $ \i -> rep n $ \j -> when (j .&. i == 0) $ do
    item <- VUM.unsafeRead g (j .|. i)
    VUM.unsafeModify g (subtract item) j
  return g

-- 高速ゼータ変換(下位集合)
fztL :: VU.Vector Int -> VU.Vector Int
fztL f' = VU.create $ do
  let
    !f = growVU f'
    !n = VU.length f
  g <- VU.unsafeThaw f
  rep' n $ \i -> rep n $ \j -> when (j .&. i == 0) $ do
    item <- VUM.unsafeRead g j
    VUM.unsafeModify g (+ item) (j .|. i)
  return g

-- 高速メビウス変換(下位集合)
fmtL :: VU.Vector Int -> VU.Vector Int
fmtL f' = VU.create $ do
  let
    !f = growVU f'
    !n = VU.length f
  g <- VU.unsafeThaw f
  rep' n $ \i -> rep n $ \j -> when (j .&. i == 0) $ do
    item <- VUM.unsafeRead g j
    VUM.unsafeModify g (subtract item) (j .|. i)
  return g

-- xor畳み込み順変換
fwt :: VU.Vector Int -> VU.Vector Int
fwt f' = VU.create $ do
  let
    !f = growVU f'
    !n = VU.length f
  g <- VU.unsafeThaw f
  rep' n $ \i -> rep n $ \j -> when (j .&. i == 0) $ do
    itemX <- VUM.unsafeRead g j
    itemY <- VUM.unsafeRead g (j .|. i)
    VUM.unsafeWrite g j (itemX + itemY)
    VUM.unsafeWrite g (j .|. i) (itemX - itemY)
  return g

-- xor畳み込み逆変換
ifwt :: VU.Vector Int -> VU.Vector Int
ifwt f' = VU.create $ do
  let
    !f = growVU f'
    !n = VU.length f
  g <- VU.unsafeThaw f
  rep' n $ \i -> rep n $ \j -> when (j .&. i == 0) $ do
    itemX <- VUM.unsafeRead g j
    itemY <- VUM.unsafeRead g (j .|. i)
    VUM.unsafeWrite g j ((itemX + itemY) `div` 2)
    VUM.unsafeWrite g (j .|. i) ((itemX - itemY) `div` 2)
  return g

-- 約数による畳み込み順変換
fdt :: VU.Vector Int -> VU.Vector Int
fdt f = VU.create $ do
  let !n = VU.length f
  g <- VU.unsafeThaw f
  sieve <- VUM.replicate n True
  repD n $ \p -> do
    check <- VUM.unsafeRead sieve p
    when check $ repD' n p $ \k -> do
      VUM.unsafeWrite sieve (k * p) False
      VUM.unsafeModify g (+ f VU.! k) (k * p)
  return g

-- 約数による畳み込み逆変換
ifdt :: VU.Vector Int -> VU.Vector Int
ifdt f = VU.create $ do
  let !n = VU.length f
  g <- VU.unsafeThaw f
  sieve <- VUM.replicate n True
  repD n $ \p -> do
    check <- VUM.unsafeRead sieve p
    when check $ revD n p $ \k -> do
      VUM.unsafeWrite sieve (k * p) False
      VUM.unsafeModify g (subtract (f VU.! k)) (k * p)
  return g

growVU :: VU.Vector Int -> VU.Vector Int
growVU v
  | VU.null v = VU.singleton 0
  | VU.length v == 1 = v
  | otherwise = v VU.++ VU.replicate (ceilPow2 n - n) 0
  where !n = VU.length v

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

repD :: Monad m => Int -> (Int -> m ()) -> m ()
repD n = flip VFSM.mapM_ (stream 2 n)
{-# INLINE repD #-}

streamD :: Monad m => Int -> Int -> Int -> VFSM.Stream m Int
streamD !l !r !p = VFSM.Stream step l
  where
    step x
      | x * p < r = return $ VFSM.Yield x (x + 1)
      | otherwise = return VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] streamD #-}

repD' :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
repD' n p = flip VFSM.mapM_ (streamD 1 n p)
{-# INLINE repD' #-}

stream' :: Monad m => Int -> Int -> VFSM.Stream m Int
stream' !l !r = VFSM.Stream step l
  where
    step x
      | x < r     = return $ VFSM.Yield x (x .<<. 1)
      | otherwise = return VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] stream' #-}

rep' :: Monad m => Int -> (Int -> m ()) -> m ()
rep' n = flip VFSM.mapM_ (stream' 1 n)
{-# INLINE rep' #-}

streamRD :: Monad m => Int -> Int -> VFSM.Stream m Int
streamRD !l !r = VFSM.Stream step r
  where
    step x
      | x > l     = return $ VFSM.Yield x (x - 1)
      | otherwise = return VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] streamRD #-}

revD :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
revD n p = flip VFSM.mapM_ (streamRD 0 ((n - 1) `div` p))
{-# INLINE revD #-}

infixl 8 .<<., .>>., .>>>.
infixl 6 .^.
(.<<.), (.>>.) :: Bits b => b -> Int -> b
(.<<.) = unsafeShiftL
{-# INLINE (.<<.) #-}
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
