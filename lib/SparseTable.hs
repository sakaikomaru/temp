{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module SparseTable where

import           Data.Bits
import           Data.Coerce
import           Data.Semigroup
import           Data.Word
import           Unsafe.Coerce
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as VU

newtype SparseTable (f :: * -> *) a = SparseTable { getSparseTable :: V.Vector (VU.Vector a) }
  deriving (Eq, Show)

buildSparseTable :: forall (f :: * -> *) a . (VU.Unbox a, Semigroup (f a), Coercible (f a) a) => VU.Vector a -> SparseTable f a
buildSparseTable vec = SparseTable . V.scanl' (\acc i -> VU.zipWith (coerce ((<>) @(f a))) acc $ VU.drop i acc) vec $ V.iterateN (floorLog2 $ VU.length vec) (*2) 1

readSparseTable :: VU.Unbox a => SparseTable f a -> Int -> a
readSparseTable st = VU.unsafeIndex (V.unsafeIndex (getSparseTable st) 0)

querySparseTable :: forall (f :: * -> *) a. (VU.Unbox a, Semigroup (f a), Coercible (f a) a) => SparseTable f a -> Int -> Int -> a
querySparseTable st l r = coerce ((<>) @(f a)) x y
  where
    logStep = floorLog2 $ r - l
    row     = V.unsafeIndex (getSparseTable st) logStep
    x       = VU.unsafeIndex row l
    y       = VU.unsafeIndex row $ r - 1 .<<. logStep


type RMQ a = SparseTable Min a

buildRMQ :: (VU.Unbox a, Ord a) => VU.Vector a -> RMQ a
buildRMQ = buildSparseTable
{-# INLINE buildRMQ #-}

readRMQ :: VU.Unbox a => RMQ a -> Int -> a
readRMQ = readSparseTable
{-# INLINE readRMQ #-}

queryMin :: (VU.Unbox a, Ord a) => RMQ a -> Int -> Int -> a
queryMin = querySparseTable
{-# INLINE queryMin #-}


floorLog2 :: Int -> Int
floorLog2 x = fromIntegral $ y .>>. 52 - 1023
  where
    y :: Word64
    y = unsafeCoerce (fromIntegral x :: Double)

infixl 8 .<<., .>>.

(.<<.) :: Bits b => b -> Int -> b
(.<<.) = unsafeShiftL
{-# INLINE (.<<.) #-}

(.>>.) :: Bits b => b -> Int -> b
(.>>.) = unsafeShiftR
{-# INLINE (.>>.) #-}