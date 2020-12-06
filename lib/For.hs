{-# LANGUAGE BangPatterns #-}

module For where

import           Control.Monad.Cont
import           Control.Monad.ST
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM

-------------------------------------------------------------------------------
-- for
-------------------------------------------------------------------------------
{-
rep      => [0 .. (n - 1)]
rep'     => [0 .. n]
rep1     => [1 .. (n - 1)]
rep1'    => [1 .. n]
rev      => [(n - 1) .. 0]
rev'     => [n .. 0]
rev1     => [(n - 1) .. 1]
rev1'    => [n .. 1]
range    => for (int i = l; i <= r; i++);
rangeR   => for (int i = r; i >= l; i--);
forStep  => for (int i = l; i <= r; i += d);
forStepR => for (int i = r; i >= l; i -= d);
forP     => for (int i = 2; i * i <= p; i++);
forG     => for (int i = l; f(i, p) <= r; g(i,d));
forRG    => for (int i = r; f(i, p) >= l; g(i,d));
-}
rep :: Monad m => Int -> (Int -> m ()) -> m ()
rep n = flip VFSM.mapM_ (stream 0 n)
{-# INLINE rep #-}

rep' :: Monad m => Int -> (Int -> m ()) -> m ()
rep' n = flip VFSM.mapM_ (stream 0 (n + 1))
{-# INLINE rep' #-}

rep1 :: Monad m => Int -> (Int -> m ()) -> m ()
rep1 n = flip VFSM.mapM_ (stream 1 n)
{-# INLINE rep1 #-}

rep1' :: Monad m => Int -> (Int -> m ()) -> m ()
rep1' n = flip VFSM.mapM_ (stream 1 (n + 1))
{-# INLINE rep1' #-}

rev :: Monad m => Int -> (Int -> m ()) -> m ()
rev n = flip VFSM.mapM_ (streamR 0 n)
{-# INLINE rev #-}

rev' :: Monad m => Int -> (Int -> m ()) -> m ()
rev' n = flip VFSM.mapM_ (streamR 0 (n + 1))
{-# INLINE rev' #-}

rev1 :: Monad m => Int -> (Int -> m ()) -> m ()
rev1 n = flip VFSM.mapM_ (streamR 1 n)
{-# INLINE rev1 #-}

rev1' :: Monad m => Int -> (Int -> m ()) -> m ()
rev1' n = flip VFSM.mapM_ (streamR 1 (n + 1))
{-# INLINE rev1' #-}

range :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
range l r = flip VFSM.mapM_ (stream l (r + 1))
{-# INLINE range #-}

rangeR :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
rangeR r l = flip VFSM.mapM_ (streamR l (r + 1))
{-# INLINE rangeR #-}

forStep :: Monad m => Int -> Int -> Int -> (Int -> m ()) -> m ()
forStep l r d = flip VFSM.mapM_ (streamStep l r d)
{-# INLINE forStep #-}

forStepR :: Monad m => Int -> Int -> Int -> (Int -> m ()) -> m ()
forStepR r l d = flip VFSM.mapM_ (streamStepR l r d)
{-# INLINE forStepR #-}

forP :: Monad m => Int -> (Int -> m ()) -> m ()
forP p = flip VFSM.mapM_ (streamG 2 p (^) 2 (+) 1)
{-# INLINE forP #-}

forG :: Monad m => Int -> Int -> (Int -> Int -> Int) -> Int -> (Int -> Int -> Int) -> Int -> (Int -> m ()) -> m ()
forG l r f p g d = flip VFSM.mapM_ (streamG l r f p g d)
{-# INLINE forG #-}

forRG :: Monad m => Int -> Int -> (Int -> Int -> Int) -> Int -> (Int -> Int -> Int) -> Int -> (Int -> m ()) -> m ()
forRG r l f p g d = flip VFSM.mapM_ (streamRG r l f p g d)
{-# INLINE forRG #-}

stream :: Monad m => Int -> Int -> VFSM.Stream m Int
stream !l !r = VFSM.Stream step l
  where
    step x
      | x < r     = return $ VFSM.Yield x (x + 1)
      | otherwise = return VFSM.Done
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

streamStep :: Monad m => Int -> Int -> Int -> VFSM.Stream m Int
streamStep !l !r !d = VFSM.Stream step l
  where
    step x
      | x <= r    = return $ VFSM.Yield x (x + d)
      | otherwise = return VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] streamStep #-}

streamStepR :: Monad m => Int -> Int -> Int -> VFSM.Stream m Int
streamStepR !l !r !d = VFSM.Stream step r
  where
    step x
      | x >= l    = return $ VFSM.Yield x (x - d)
      | otherwise = return VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] streamStepR #-}

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

