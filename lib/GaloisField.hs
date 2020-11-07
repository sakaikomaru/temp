{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module GaloisField where

import           Data.Coerce
import           Data.Proxy
import           GHC.TypeLits
import qualified Data.Vector.Generic         as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

test :: IO ()
test = do
  let gf1 = mkGF 4322532 :: GF 1_000_000_007
  let gf2 = mkGF 233 :: GF 1_000_000_007
  print $ gf1 * gf2
  print $ (100001023 :: GF 1_000_000_007) * (123983232718 :: GF 1_000_000_007)
  let xs = VU.fromList $ ([432,24398,23478,1781,13289,32489,4398,324891,43278,112187278,437289,32784] :: [GF 1_000_000_007])
  print $ VU.map (* 1_237_811_453) xs
  reifyModulus 998244353 $ \proxy -> print (modulusVal proxy)
  print $ VU.map (* 1_237_811_453) xs
  
newtype GF (p :: Nat) = GF { unGF :: Int }
  deriving Eq
  deriving newtype Show

modulusVal :: KnownNat p => Proxy p -> Int
modulusVal = fromIntegral . natVal
{-# INLINE modulusVal #-}

mkGF :: forall p. KnownNat p => Int -> GF p
mkGF x = GF (x `mod` modulusVal (Proxy @p))

reifyModulus :: Integral int => int -> (forall p. KnownNat p => Proxy p -> a) -> a
reifyModulus n f = case someNatVal (fromIntegral n) of
    Just (SomeNat proxy) -> f proxy
    Nothing              -> error "reifyModulus failed"

instance (KnownNat p) => Num (GF p) where
    x + y = case coerce x + coerce y of
              xy
                | xy < m -> coerce xy
                | otherwise -> coerce (xy - m)
      where
        m = modulusVal (Proxy @p)
    x - y = case coerce x - coerce y of
              xy
                | xy < 0 -> coerce $ xy + modulusVal (Proxy @p)
                | otherwise -> coerce xy
    x * y = GF $ coerce x * coerce y `rem` modulusVal (Proxy @p)
    abs = id
    signum = const (GF 1)
    fromInteger x = GF . fromIntegral $ x `mod` fromIntegral m
      where
        m = modulusVal (Proxy @p)

instance (KnownNat p) => Fractional (GF p) where
    recip x = coerce $ go (coerce x) m 1 0
      where
        !m = modulusVal (Proxy @p)
        go !a !b !u !v
            | b > 0 = case a `quot` b of
                q -> go b (a - (q * b)) v (u - (q * v))
            | otherwise = u `mod` m
    fromRational _ = undefined

newtype instance VUM.MVector s (GF _) = MV_GF (VUM.MVector s Int)
newtype instance VU.Vector (GF _)= V_GF (VU.Vector Int)

instance VU.Unbox (GF p)

instance VGM.MVector VUM.MVector (GF p) where
    basicLength (MV_GF v) = VGM.basicLength v
    {-# INLINE basicLength #-}
    basicUnsafeSlice i n (MV_GF v) = MV_GF $ VGM.basicUnsafeSlice i n v
    {-# INLINE basicUnsafeSlice #-}
    basicOverlaps (MV_GF v1) (MV_GF v2) = VGM.basicOverlaps v1 v2
    {-# INLINE basicOverlaps #-}
    basicUnsafeNew n = MV_GF `fmap` VGM.basicUnsafeNew n
    {-# INLINE basicUnsafeNew #-}
    basicInitialize (MV_GF v) = VGM.basicInitialize v
    {-# INLINE basicInitialize #-}
    basicUnsafeReplicate n x = MV_GF `fmap` VGM.basicUnsafeReplicate n (coerce x)
    {-# INLINE basicUnsafeReplicate #-}
    basicUnsafeRead (MV_GF v) i = coerce `fmap` VGM.basicUnsafeRead v i
    {-# INLINE basicUnsafeRead #-}
    basicUnsafeWrite (MV_GF v) i x = VGM.basicUnsafeWrite v i (coerce x)
    {-# INLINE basicUnsafeWrite #-}
    basicClear (MV_GF v) = VGM.basicClear v
    {-# INLINE basicClear #-}
    basicSet (MV_GF v) x = VGM.basicSet v (coerce x)
    {-# INLINE basicSet #-}
    basicUnsafeCopy (MV_GF v1) (MV_GF v2) = VGM.basicUnsafeCopy v1 v2
    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeMove (MV_GF v1) (MV_GF v2) = VGM.basicUnsafeMove v1 v2
    {-# INLINE basicUnsafeMove #-}
    basicUnsafeGrow (MV_GF v) n = MV_GF `fmap` VGM.basicUnsafeGrow v n
    {-# INLINE basicUnsafeGrow #-}

instance VG.Vector VU.Vector (GF p) where
    basicUnsafeFreeze (MV_GF v) = V_GF `fmap` VG.basicUnsafeFreeze v
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeThaw (V_GF v) = MV_GF `fmap` VG.basicUnsafeThaw v
    {-# INLINE basicUnsafeThaw #-}
    basicLength (V_GF v) = VG.basicLength v
    {-# INLINE basicLength #-}
    basicUnsafeSlice i n (V_GF v) = V_GF $ VG.basicUnsafeSlice i n v
    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeIndexM (V_GF v) i = coerce `fmap` VG.basicUnsafeIndexM v i
    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeCopy (MV_GF mv) (V_GF v) = VG.basicUnsafeCopy mv v
    elemseq _ = seq
    {-# INLINE elemseq #-}