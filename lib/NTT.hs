{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UnboxedTuples        #-}
{-# LANGUAGE UndecidableInstances #-}

module NTT where

import           Control.Monad
import           Control.Monad.ST
import           Data.Bits
import           GHC.Exts
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM
import qualified Data.Vector.Unboxed               as VU
import qualified Data.Vector.Unboxed.Mutable       as VUM
import qualified GHC.Integer.GMP.Internals         as GMP

data NTTRunner = NTTRunner
  { pNR    :: !Int
  , gNR    :: !Int
  , ipNR   :: !Word
  , sesNR  :: !(VU.Vector Int)
  , siesNR :: !(VU.Vector Int)
  }

buildNTTRunner :: Int -> Int -> NTTRunner
buildNTTRunner pNR gNR = NTTRunner{..}
  where
    ipNR = quot (complement 0) (fromIntegral pNR) + 1
    ctz = countTrailingZeros (pNR - 1)
    !e  = powModInt gNR ((pNR - 1) .>>. ctz) pNR
    !ie = recipModInt e pNR
    es  = VU.reverse $ VU.iterateN (ctz - 1) (\x -> x *% x) e
    ies = VU.reverse $ VU.iterateN (ctz - 1) (\x -> x *% x) ie
    sesNR  = VU.zipWith (*%) es  $ VU.scanl' (*%) 1 ies
    siesNR = VU.zipWith (*%) ies $ VU.scanl' (*%) 1 es
    x *% y = x * y `rem` pNR

addNR :: NTTRunner -> Int -> Int -> Int
addNR (NTTRunner (I# m#) _ _ _ _) (I# x#) (I# y#)
  = case x# +# y# of
    z# -> I# (z# -# ((z# >=# m#) *# m#))
{-# INLINE addNR #-}

subNR :: NTTRunner -> Int -> Int -> Int
subNR (NTTRunner (I# m#) _ _ _ _) (I# x#) (I# y#)
  = case x# -# y# of
    z# -> I# (z# +# ((z# <# 0#) *# m#))
{-# INLINE subNR #-}

mulNR :: NTTRunner -> Int -> Int -> Int
mulNR (NTTRunner (I# m0#) _ (W# im#) _ _) (I# x#) (I# y#)
  = case timesWord# (int2Word# x#) (int2Word# y#) of
      z# -> case timesWord2# z# im# of
        (# q#, _ #) -> case minusWord# z# (timesWord# q# m#) of
          v#  | isTrue# (geWord# v# m#) -> I# (word2Int# (plusWord# v# m#))
              | otherwise -> I# (word2Int# v#)
  where
    m# = int2Word# m0#
{-# INLINE mulNR #-}

butterfly :: NTTRunner -> VUM.STVector s Int -> ST s ()
butterfly nr@NTTRunner{..} mvec = do
  flip VFSM.mapM_ (stream 1 (h + 1)) $ \ph -> do
    let !w = 1 .<<. (ph - 1)
        !p = 1 .<<. (h - ph)
    void $ VFSM.foldlM'
      (\acc s -> do
        let offset = s .<<. (h - ph + 1)
        flip VFSM.mapM_ (stream offset (offset + p)) $ \i -> do
          l <- VUM.unsafeRead mvec i
          r <- mulNR nr acc <$!> VUM.unsafeRead mvec (i + p)
          VUM.unsafeWrite mvec (i + p) $ subNR nr l r
          VUM.unsafeWrite mvec i $ addNR nr l r
        return $! mulNR nr acc $ VU.unsafeIndex siesNR (countTrailingZeros (complement s))
      ) 1 (stream 0 w)
    where
      n  = VUM.length mvec
      !h = ctzceilpow2 n
{-# INLINE butterfly #-}

invButterfly :: NTTRunner -> VUM.STVector s Int -> ST s ()
invButterfly nr@NTTRunner{..} mvec = void $ do
  flip VFSM.mapM_ (streamR 1 (h + 1)) $ \ph -> do
    let !w = 1 .<<. (ph - 1)
        !p = 1 .<<. (h - ph)
    VFSM.foldlM'
      (\acc s -> do
        let offset = s .<<. (h - ph + 1)
        flip VFSM.mapM_ (stream offset (offset + p)) $ \i -> do
          l <- VUM.unsafeRead mvec i
          r <- VUM.unsafeRead mvec (i + p)
          VUM.unsafeWrite mvec (i + p) $ mulNR nr acc (pNR + l - r)
          VUM.unsafeWrite mvec i $ addNR nr l r
        return $! mulNR nr acc $ VU.unsafeIndex sesNR (countTrailingZeros (complement s))
      ) 1 (stream 0 w)
  where
    n  = VUM.length mvec
    !h = ctzceilpow2 n
{-# INLINE invButterfly #-}

growToPowerOfTwo :: VU.Vector Int -> VU.Vector Int
growToPowerOfTwo v
  | VU.null v = VU.singleton 0
  | VU.length v == 1 = v
  | n <- (-1) .>>>. (countTrailingZeros (VU.length v - 1)) + 1
    = v VU.++ VU.replicate (n - VU.length v) 0

ntt :: Int -> Int -> VU.Vector Int -> VU.Vector Int
ntt p g = VU.modify (butterfly nr)
  where
    nr = buildNTTRunner p g

intt :: Int -> Int -> VU.Vector Int -> VU.Vector Int
intt p g f = VU.map (mulNR nr invn) $ VU.modify (invButterfly nr) f
  where
    nr    = buildNTTRunner p g
    !invn = recipModInt (VU.length f) p

convolute :: Int -> Int -> VU.Vector Int -> VU.Vector Int -> VU.Vector Int
convolute p g xs ys = VU.create $ do
  mxs <- VUM.replicate len 0
  VU.unsafeCopy (VUM.take n mxs) xs
  butterfly nr mxs
  mys <- VUM.replicate len 0
  VU.unsafeCopy (VUM.take m mys) ys
  butterfly nr mys
  rep len $ \i -> do
    yi <- VUM.unsafeRead mys i
    VUM.unsafeModify mxs (mulNR nr yi) i
  invButterfly nr mxs
  rep (n + m - 1) $ \i -> do
    VUM.unsafeModify mxs (mulNR nr ilen) i
  return $ VUM.take (n + m - 1) mxs
    where
      !nr   = buildNTTRunner p g
      n     = VU.length xs
      m     = VU.length ys
      !h    = head [i | i <- [0..], n + m - 1 <= 1 .<<. i]
      !len  = 1 .<<. h
      !ilen = recipModInt len p

multiply :: VU.Vector Int -> VU.Vector Int -> Int -> VU.Vector Int
multiply ax bx mo = ret
  where
    m1 = 167772161
    m2 = 469762049
    m3 = 1224736769
    g  = 3
    x  = convolute m1 g ax bx
    y  = convolute m2 g ax bx
    z  = convolute m3 g ax bx
    !m1invm2  = recipModInt m1 m2
    !m12invm3 = recipModInt (m1 * m2) m3
    !m12mod   = m1 * m2 `mod` mo
    !v1 = VU.zipWith func1 y x
    func1 !yi !xi =
      let c = (yi - xi) * m1invm2 `mod` m2
      in if c < 0 then c + m2 else c
    !v2 = VU.zipWith3 func2 z x v1
    func2 !zi !xi !v1i =
      let d = (zi - (xi + m1 * v1i) `mod` m3) * m12invm3 `mod` m3
      in if d < 0 then d + m3 else d
    !ret = VU.zipWith3 func3 x v1 v2
    func3 !xi !v1i !v2i =
      let e = (xi + m1 * v1i + m12mod * v2i) `mod` mo
      in if e < 0 then e + mo else e

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
      | otherwise = return $ VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] stream #-}

streamR :: Monad m => Int -> Int -> VFSM.Stream m Int
streamR !l !r = VFSM.Stream step (r - 1)
  where
    step x
      | x >= l = return $ VFSM.Yield x (x - 1)
      | otherwise = return VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] streamR #-}

rep :: Monad m => Int -> (Int -> m ()) -> m ()
rep n = flip VFSM.mapM_ (stream 0 n)
{-# INLINE rep #-}

rev :: Monad m => Int -> (Int -> m ()) -> m ()
rev n = flip VFSM.mapM_ (streamR 0 n)
{-# INLINE rev #-}

rev1 :: Monad m => Int -> (Int -> m ()) -> m ()
rev1 n = flip VFSM.mapM_ (streamR 1 (n + 1))
{-# INLINE rev1 #-}

streamStep :: Monad m => Int -> Int -> Int -> VFSM.Stream m Int
streamStep !l !r !d = VFSM.Stream step l
  where
    step x
      | x < r = return $ VFSM.Yield x (x + d)
      | otherwise = return VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] streamStep #-}

encode32x2 :: Int -> Int -> Int
encode32x2 x y = x .<<. 32 .|. y
{-# INLINE encode32x2 #-}

decode32x2 :: Int -> (Int, Int)
decode32x2 xy =
    let !x = xy .>>>. 32
        !y = xy .&. 0xffffffff
    in (x, y)
{-# INLINE decode32x2 #-}

fi :: Int -> Integer
fi = fromIntegral
{-# INLINE fi #-}

fI :: Integer -> Int
fI = fromInteger
{-# INLINE fI #-}

powModInt :: Int -> Int -> Int -> Int
powModInt a n mo = fI $ GMP.powModInteger (fi a) (fi n) (fi mo)
{-# INLINE powModInt #-}

recipModInt :: Int -> Int -> Int
recipModInt a mo = fI $ GMP.recipModInteger (fi a) (fi mo)

ceilPow2 :: Int -> Int
ceilPow2 n
  | n > 1     = (-1) .>>>. (countLeadingZeros (n - 1)) + 1
  | otherwise = 1

floorPow2 :: Int -> Int
floorPow2 n
  | n >= 1    = 1 .<<. (63 - (countLeadingZeros n))
  | otherwise = 0

ctzceilpow2 :: Int -> Int
ctzceilpow2 = countTrailingZeros . ceilPow2
