{-# LANGUAGE BangPatterns #-}

module EditDistance where

import           Control.Monad.ST
import           Data.Bool
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM
import qualified Data.Vector.Unboxed               as VU
import qualified Data.Vector.Unboxed.Mutable       as VUM

levenshteinDistance :: VU.Vector Char -> VU.Vector Char -> Int
levenshteinDistance s t = runST $ do
  let !m = VU.length s
  let !n = VU.length t
  dp <- VUM.replicate ((m + 1) * (n + 1)) (1000000007 :: Int)
  rep' m $ \i -> VUM.unsafeWrite dp (i * (n + 1)) i
  rep' n $ \j -> VUM.unsafeWrite dp j j
  rep  m $ \i -> rep n $ \j -> do
    item1 <- VUM.unsafeRead dp (i * (n + 1) + j)
    item2 <- VUM.unsafeRead dp (i * (n + 1) + (j + 1))
    item3 <- VUM.unsafeRead dp ((i + 1) * (n + 1) + j)
    VUM.unsafeWrite dp ((i + 1) * (n + 1) + (j + 1)) (minimum [item1 + bool 0 1 (s VU.! i /= t VU.! j), item2 + 1, item3 + 1])
  VUM.unsafeRead dp (m * (n + 1) + n)

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

rep' :: Monad m => Int -> (Int -> m ()) -> m ()
rep' n = flip VFSM.mapM_ (stream 0 (n + 1))
{-# INLINE rep' #-}