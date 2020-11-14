{-# LANGUAGE BangPatterns #-}

module For where

import           Control.Monad.Cont
import           Control.Monad.ST
import           Data.Bits
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM

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

rep1 :: Monad m => Int -> (Int -> m ()) -> m ()
rep1 n = flip VFSM.mapM_ (stream 1 (n + 1))
{-# INLINE rep1 #-}

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

rev' :: Monad m => Int -> (Int -> m ()) -> m ()
rev' n = flip VFSM.mapM_ (streamR 0 (n + 1))
{-# INLINE rev' #-}

rev1 :: Monad m => Int -> (Int -> m ()) -> m ()
rev1 n = flip VFSM.mapM_ (streamR 1 (n + 1))
{-# INLINE rev1 #-}

streamStep :: Monad m => Int -> Int -> Int -> VFSM.Stream m Int
streamStep !l !r !d = VFSM.Stream step l
  where
    step x
      | x < r     = return $ VFSM.Yield x (x + d)
      | otherwise = return VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] streamStep #-}

repStep :: Monad m => Int -> Int -> Int -> (Int -> m ()) -> m ()
repStep l r d = flip VFSM.mapM_ (streamStep l r d)
{-# INLINE repStep #-}

repStep' :: Monad m => Int -> Int -> Int -> (Int -> m ()) -> m ()
repStep' l r d = flip VFSM.mapM_ (streamStep l (r + 1) d)
{-# INLINE repStep' #-}

streamHalf :: Monad m => Int -> Int -> VFSM.Stream m Int
streamHalf !l !r = VFSM.Stream step r
  where
    step x
      | x > l     = return $ VFSM.Yield x (x `div` 2)
      | otherwise = return VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] streamHalf #-}

rehf :: Monad m => Int -> (Int -> m ()) -> m ()
rehf n = flip VFSM.mapM_ (streamHalf 0 n)
{-# INLINE rehf #-}

streamBit :: Monad m => Int -> Int -> VFSM.Stream m Int
streamBit !l !r = VFSM.Stream step l
  where
    step x
      | x < (1 .<<. r) = return $ VFSM.Yield x (x + 1)
      | otherwise      = return VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] streamBit #-}

repBit :: Monad m => Int -> (Int -> m ()) -> m ()
repBit n = flip VFSM.mapM_ (streamBit 0 n)
{-# INLINE repBit #-}

infixl 8 .<<., .>>.

(.<<.) :: Bits b => b -> Int -> b
(.<<.) = unsafeShiftL
{-# INLINE (.<<.) #-}

(.>>.) :: Bits b => b -> Int -> b
(.>>.) = unsafeShiftR
{-# INLINE (.>>.) #-}

withBreakIO :: ((r -> ContT r IO b) -> ContT r IO r) -> IO r
withBreakIO = flip runContT pure . callCC
{-# INLINE withBreakIO #-}

withBreakST :: ((r -> ContT r (ST s) b) -> ContT r (ST s) r) -> (ST s) r
withBreakST = flip runContT pure . callCC
{-# INLINE withBreakST #-}