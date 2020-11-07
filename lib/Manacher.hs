{-# LANGUAGE BangPatterns #-}

module Manacher where

import           Control.Monad
import           Control.Monad.Fix
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Unsafe      as BSU
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

manacher :: BS.ByteString -> VU.Vector Int
manacher bs = VU.create $ do
  rad <- VUM.replicate n 0
  fix `flip` 0 `flip` 0 $ \loop !center !radius -> do
    when (center < n) $ do
      let !radius' = naiveRadius center radius
      VUM.unsafeWrite rad center radius'
      flip fix 1 $ \inner !r -> do
        if center >= r && center + r < n
          then do
            radL <- VUM.unsafeRead rad (center - r)
            if r + radL < radius'
              then do
                VUM.unsafeWrite rad (center + r) radL
                inner (r + 1)
              else loop (center + r) (radius' - r)
          else loop (center + r) (radius' - r)
  return rad
  where
    !n = BS.length bs
    naiveRadius :: Int -> Int -> Int
    naiveRadius c r = go r
      where
        go !i
          | c - i >= 0, c + i < n
          , BSU.unsafeIndex bs (c - i) == BSU.unsafeIndex bs (c + i) = go (i + 1)
          | otherwise = i
