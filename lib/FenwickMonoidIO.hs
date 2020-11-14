{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module FenwickMonoidIO where

import           Control.Monad
import           Control.Monad.Fix
import           Data.Bits
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

newtype FenwickMonoidTree a = FenwickMonoidTree { getFenwickMonoidTree :: VUM.IOVector a }

newFenwickMonoidTree :: (Monoid a, VU.Unbox a) => Int -> IO (FenwickMonoidTree a)
newFenwickMonoidTree n = FenwickMonoidTree <$> VUM.replicate (n + 1) mempty
{-# INLINE newFenwickMonoidTree #-}

buildFenwickMonoidTree :: (Monoid a, VU.Unbox a) => VU.Vector a -> IO (FenwickMonoidTree a)
buildFenwickMonoidTree vec = do
  let n = VU.length vec
  fmt <- VUM.unsafeNew (n + 1)
  VUM.unsafeWrite fmt 0 mempty
  VU.unsafeCopy (VUM.tail fmt) vec
  flip fix 1 $ \loop !i -> when (i <= n) $ do
    let j = i + (i .&. (-i))
    when (j <= n) $ do
      fmti <- VUM.unsafeRead fmt i
      VUM.unsafeModify fmt (<> fmti) j
    loop (i + 1)
  return $ FenwickMonoidTree fmt
{-# INLINE buildFenwickMonoidTree #-}

infixl 9 <<>>!, <<>>!!

-- | 0-indexed
(<<>>!!) :: (Monoid a, VU.Unbox a) => FenwickMonoidTree a -> Int -> IO a
fmt <<>>!! i = fmt <<>>! (i + 1)
{-# INLINE (<<>>!!) #-}

-- | 1-indexed
(<<>>!) :: (Monoid a, VU.Unbox a) => FenwickMonoidTree a -> Int -> IO a
(<<>>!) (FenwickMonoidTree fmt) = go mempty
  where
    go !acc !i
      | i > 0 = do
          xi <- VUM.unsafeRead fmt i
          go (acc <> xi) (i - (i .&. (-i)))
      | otherwise = return acc
{-# INLINE (<<>>!) #-}

-- | 0-indexed
mappendAt' :: (Semigroup a, VU.Unbox a) => FenwickMonoidTree a -> Int -> a -> IO ()
mappendAt' (FenwickMonoidTree fmt) k v = flip fix (k + 1) $ \loop !i -> do
  when (i < n) $ do
    VUM.unsafeModify fmt (<> v) i
    loop (i + (i .&. (-i)))
  where !n = VUM.length fmt
{-# INLINE mappendAt' #-}

-- | 1-indexed
mappendAt :: (Semigroup a, VU.Unbox a) => FenwickMonoidTree a -> Int -> a -> IO ()
mappendAt (FenwickMonoidTree fmt) k v = flip fix k $ \loop !i -> do
  when (i < n) $ do
    VUM.unsafeModify fmt (<> v) i
    loop (i + (i .&. (-i)))
  where !n = VUM.length fmt
{-# INLINE mappendAt #-}