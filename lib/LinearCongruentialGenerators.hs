{-# LANGUAGE BangPatterns #-}

module LinearCongruentialGenerators where

import           System.CPUTime
import           Unsafe.Coerce
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM
import qualified Data.Vector.Unboxed               as VU
import qualified Data.Vector.Unboxed.Mutable       as VUM

type RNG = VUM.IOVector Int

newRNG :: IO RNG
newRNG = do
  rng  <- VUM.unsafeNew 1
  seed <- fI <$> getCPUTime
  VUM.unsafeWrite rng 0 seed
  return rng

_getNextRnd :: Integer -> Integer
_getNextRnd n = (n * 307139 + 17) `mod` 891771648051262478234242325342735462117
{-# INLINE _getNextRnd #-}

fi :: Int -> Integer
fi = fromIntegral
{-# INLINE fi #-}

fI :: Integer -> Int
fI = fromInteger
{-# INLINE fI #-}

getNextRnd :: Int -> Int
getNextRnd = flip mod (maxBound :: Int) . fI . _getNextRnd . fi
{-# INLINE getNextRnd #-}

nextInt :: RNG -> IO Int
nextInt rng = do
  next <- VUM.unsafeRead rng 0
  VUM.unsafeWrite rng 0 (getNextRnd next)
  return next

nextWord :: RNG -> IO Word
nextWord = unsafeCoerce <$> nextInt

randomR :: RNG -> Int -> Int -> IO Int
randomR rng l r = do
  x <- nextInt rng
  return $ x `mod` (r - l + 1) + l

shuffleM :: VUM.Unbox a => RNG -> VUM.IOVector a -> IO ()
shuffleM rng mvec = do
  rev (VUM.length mvec) $ \i -> do
    j <- nextWord rng
    VUM.unsafeSwap mvec i (unsafeCoerce $ rem j (unsafeCoerce i + 1))

shuffle :: VU.Unbox a => RNG -> VU.Vector a -> IO (VU.Vector a)
shuffle rng vec = do
  mv <- VU.unsafeThaw vec
  shuffleM rng mv
  VU.unsafeFreeze mv

streamR :: Monad m => Int -> Int -> VFSM.Stream m Int
streamR !l !r = VFSM.Stream step (r - 1)
  where
    step x
      | x >= l    = return $ VFSM.Yield x (x - 1)
      | otherwise = return VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] streamR #-}

rev :: Monad m => Int -> (Int -> m ()) -> m ()
rev n = flip VFSM.mapM_ (streamR 0 n)
{-# INLINE rev #-}