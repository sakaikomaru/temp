module PrimeTable where

import           Control.Monad
import           Control.Monad.ST
import           Data.Bits
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

primeTable :: Int -> VU.Vector Int
primeTable top = VU.filter (/= -1) . VU.imap (\i check -> if i == 0 then 2 else if check then i * 2 + 1 else -1) $! runST $ do
  let m = (top - 1) `shiftR` 1
      r = floor . sqrt . fromIntegral $ (top + 1)
  sieve <- VU.unsafeThaw $ VU.replicate (m + 1) True
  forM_ [1 .. r `unsafeShiftR` 1] $ \i -> do
    isPrime <- VUM.unsafeRead sieve i
    when isPrime $ forM_ [2 * i * (i + 1), 2 * i * (i + 2) + 1 .. m] $ \j -> VUM.unsafeWrite sieve j False
  VU.unsafeFreeze sieve