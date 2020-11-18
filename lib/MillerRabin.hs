{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE OverloadedLists #-}

module MillerRabin where

import           Control.Monad
import           Control.Monad.Cont
import           Control.Monad.ST
import           Data.Bits
import           Data.STRef.Strict
import qualified GHC.Integer.GMP.Internals         as GMP
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM
import qualified Data.Vector.Unboxed               as VU

millerRabin :: Int -> Bool
millerRabin k
  | k <= 3 = k == 2 || k == 3
  | k .&. 1 == 0 = False
  | otherwise = _millerRabin k
  where
    _millerRabin :: Int -> Bool
    _millerRabin n
      | n < 2047            = loop [2]
      | n < 1373653         = loop [2,3]
      | n < 9080191         = loop [31,73]
      | n < 25326001        = loop [2,3,5]
      | n < 4759123141      = loop [2,7,61]
      | n < 1122004669633   = loop [2,13,23,1662803]
      | n < 2152302898747   = loop [2,3,5,7,11]
      | n < 3474749660383   = loop [2,3,5,7,11,13]
      | n < 341550071728321 = loop [2,3,5,7,11,13,17]
      | otherwise           = loop [2,325,9375,28178,450775,9780504,1795265022]
      where
        !m = n - 1
        !s = ctz m
        !d = m .>>. s
        loop :: VU.Vector Int -> Bool
        loop vec = runST $ do
          ret <- newSTRef True
          withBreakST $ \break1 -> do
            VU.forM_ vec $ \a -> do
              let
                check1 = powModInt a d n /= 1
                check2 = all (\r -> powModInt a ((1 .<<. r) * d) n /= m) ([0..(s - 1)] :: [Int])
              when (check1 && check2) $ do
                lift $ writeSTRef ret False
                break1 ()
          readSTRef ret

powModInt :: Int -> Int -> Int -> Int
powModInt a n mo = fI $ GMP.powModInteger (fi a) (fi n) (fi mo)
{-# INLINE powModInt #-}

recipModInt :: Int -> Int -> Int
recipModInt a mo = fI $ GMP.recipModInteger (fi a) (fi mo)
{-# INLINE recipModInt #-}

fi :: Int -> Integer
fi = fromIntegral
{-# INLINE fi #-}

fI :: Integer -> Int
fI = fromInteger
{-# INLINE fI #-}

clz :: FiniteBits fb => fb -> Int
clz = countLeadingZeros
{-# INLINE clz #-}

ctz :: FiniteBits fb => fb -> Int
ctz = countTrailingZeros
{-# INLINE ctz #-}

infixl 8 .<<., .>>.

(.<<.) :: Bits b => b -> Int -> b
(.<<.) = unsafeShiftL
{-# INLINE (.<<.) #-}

(.>>.) :: Bits b => b -> Int -> b
(.>>.) = unsafeShiftR
{-# INLINE (.>>.) #-}

rep :: Monad m => Int -> (Int -> m ()) -> m ()
rep n = flip VFSM.mapM_ (streamG 0 (n - 1) const 0 (+) 1)
{-# INLINE rep #-}

rep' :: Monad m => Int -> (Int -> m ()) -> m ()
rep' n = flip VFSM.mapM_ (streamG 0 n const 0 (+) 1)
{-# INLINE rep' #-}

rep1 :: Monad m => Int -> (Int -> m ()) -> m ()
rep1 n = flip VFSM.mapM_ (streamG 1 (n - 1) const 0 (+) 1)
{-# INLINE rep1 #-}

rep1' :: Monad m => Int -> (Int -> m ()) -> m ()
rep1' n = flip VFSM.mapM_ (streamG 1 n const 0 (+) 1)
{-# INLINE rep1' #-}

rev :: Monad m => Int -> (Int -> m ()) -> m ()
rev n = flip VFSM.mapM_ (streamRG (n - 1) 0 const 0 (-) 1)
{-# INLINE rev #-}

rev' :: Monad m => Int -> (Int -> m ()) -> m ()
rev' n = flip VFSM.mapM_ (streamRG n 0 const 0 (-) 1)
{-# INLINE rev' #-}

rev1 :: Monad m => Int -> (Int -> m ()) -> m ()
rev1 n = flip VFSM.mapM_ (streamRG (n - 1) 1 const 0 (-) 1)
{-# INLINE rev1 #-}

rev1' :: Monad m => Int -> (Int -> m ()) -> m ()
rev1' n = flip VFSM.mapM_ (streamRG n 1 const 0 (-) 1)
{-# INLINE rev1' #-}

range :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
range l r = flip VFSM.mapM_ (streamG l r const 0 (+) 1)
{-# INLINE range #-}

rangeR :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
rangeR r l = flip VFSM.mapM_ (streamRG r l const 0 (-) 1)
{-# INLINE rangeR #-}

forP :: Monad m => Int -> (Int -> m ()) -> m ()
forP p = flip VFSM.mapM_ (streamG 2 (p - 1) (^) 2 (+) 1)
{-# INLINE forP #-}

forG :: Monad m => Int -> Int -> (Int -> Int -> Int) -> Int -> (Int -> Int -> Int) -> Int -> (Int -> m ()) -> m ()
forG l r f p g d = flip VFSM.mapM_ (streamG l r f p g d)
{-# INLINE forG #-}

forRG :: Monad m => Int -> Int -> (Int -> Int -> Int) -> Int -> (Int -> Int -> Int) -> Int -> (Int -> m ()) -> m ()
forRG r l f p g d = flip VFSM.mapM_ (streamRG r l f p g d)
{-# INLINE forRG #-}

streamG :: Monad m => Int -> Int -> (Int -> Int -> Int) -> Int -> (Int -> Int -> Int) -> Int -> VFSM.Stream m Int
streamG !l !r !f !p !g !d = VFSM.Stream step l
  where
    step x
      | f x p <= r = return $ VFSM.Yield x (g x d)
      | otherwise  = return VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] streamG #-}

streamRG :: Monad m => Int -> Int -> (Int -> Int -> Int) -> Int -> (Int -> Int -> Int) -> Int -> VFSM.Stream m Int
streamRG !r !l !f !p !g !d = VFSM.Stream step r
  where
    step x
      | f x p >= l = return $ VFSM.Yield x (g x d)
      | otherwise  = return VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] streamRG #-}

withBreakIO :: ((r -> ContT r IO b) -> ContT r IO r) -> IO r
withBreakIO = flip runContT pure . callCC
{-# INLINE withBreakIO #-}

withBreakST :: ((r -> ContT r (ST s) b) -> ContT r (ST s) r) -> (ST s) r
withBreakST = flip runContT pure . callCC
{-# INLINE withBreakST #-}
