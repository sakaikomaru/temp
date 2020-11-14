{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module LazySegmentTree where

import           Control.Monad
import           Control.Monad.Fix
import           Data.Bits
import           Data.Monoid                       hiding (First (..), Last (..))
import           Data.Semigroup
import           GHC.Exts
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM
import qualified Data.Vector.Unboxed               as VU
import qualified Data.Vector.Unboxed.Mutable       as VUM

class Monoid f => MonoidAction f a where
  appMonoid :: f -> a -> a

data LazySegmentTree a f = LazySegmentTree (VUM.IOVector a) (VUM.IOVector f)

newLazySegmentTree :: (Monoid a, VU.Unbox a, Monoid f, VU.Unbox f) => Int -> IO (LazySegmentTree a f)
newLazySegmentTree n = LazySegmentTree <$> VUM.replicate (2 * extendToPowerOfTwo n) mempty <*> VUM.replicate (extendToPowerOfTwo n) mempty

buildLazySegmentTree :: (Monoid a, VU.Unbox a, Monoid f, VU.Unbox f) => VU.Vector a -> IO (LazySegmentTree a f)
buildLazySegmentTree xs = do
  tree <- VUM.replicate (2 * n) mempty
  lazy <- VUM.replicate n mempty
  VU.unsafeCopy (VUM.unsafeSlice n (VU.length xs) tree) xs
  let seg = LazySegmentTree tree lazy
  flip VFSM.mapM_ (streamR 1 n) $ \i -> updateLazySegmentTree seg i
  return seg
    where
      !n = extendToPowerOfTwo $ VU.length xs

updateLazySegmentTree :: (Monoid a, VU.Unbox a) => LazySegmentTree a f -> Int -> IO ()
updateLazySegmentTree (LazySegmentTree tree _) k = (<>) <$> VUM.unsafeRead tree (2 * k) <*> VUM.unsafeRead tree (2 * k + 1) >>= VUM.unsafeWrite tree k
{-# INLINE updateLazySegmentTree #-}

appAllAt :: (Monoid a, VU.Unbox a, Monoid f, VU.Unbox f, MonoidAction f a) => LazySegmentTree a f -> Int -> f -> IO ()
appAllAt (LazySegmentTree tree lazy) k f = do
  VUM.unsafeModify tree (appMonoid f) k
  when (k < VUM.length lazy) $ VUM.unsafeModify lazy (mappend f) k
{-# INLINE appAllAt #-}

pushLazySegmentTree :: (Monoid a, VU.Unbox a, Monoid f, VU.Unbox f, MonoidAction f a) => LazySegmentTree a f -> Int  -> IO ()
pushLazySegmentTree st@(LazySegmentTree _ lazy) k = do
  fk <- VUM.unsafeRead lazy k
  appAllAt st (2 * k) fk
  appAllAt st (2 * k + 1) fk
  VUM.unsafeWrite lazy k mempty
{-# INLINE pushLazySegmentTree #-}

writeLazySegmentTree :: (Monoid a, VU.Unbox a, Monoid f, VU.Unbox f, MonoidAction f a) => LazySegmentTree a f -> Int -> a -> IO ()
writeLazySegmentTree st@(LazySegmentTree tree lazy) k0 v = do
  let !n = VUM.length lazy
      k  = k0 + n
      !h = 64 - clz n
  flip VFSM.mapM_ (streamR 1 h) $ \i -> pushLazySegmentTree st (k .>>. i)
  VUM.unsafeWrite tree k v
  flip VFSM.mapM_ (stream 1 h)  $ \i -> updateLazySegmentTree st (k .>>. i)
{-# INLINE writeLazySegmentTree #-}

readLazySegmentTree :: (Monoid a, VU.Unbox a, Monoid f, VU.Unbox f, MonoidAction f a) => LazySegmentTree a f -> Int -> IO a
readLazySegmentTree st@(LazySegmentTree tree lazy) k0 = do
  let !n = VUM.length lazy
      k  = k0 + n
      !h = 64 - clz n
  flip VFSM.mapM_ (streamR 1 h) $ \i -> pushLazySegmentTree st (k .>>. i)
  VUM.unsafeRead tree k
{-# INLINE readLazySegmentTree #-}

mappendFromTo :: (Monoid a, VU.Unbox a, Monoid f, VU.Unbox f, MonoidAction f a) => LazySegmentTree a f -> Int -> Int -> IO a
mappendFromTo st@(LazySegmentTree tree lazy) l0 r0 = do
  let !n = VUM.length lazy
      !l = l0 + n
      !r = r0 + n
      !h = 64 - clz n
  flip VFSM.mapM_ (streamR 1 h) $ \i -> do
    when ((l .>>. i) .<<. i /= l) $ pushLazySegmentTree st (l .>>. i)
    when ((r .>>. i) .<<. i /= r) $ pushLazySegmentTree st (r .>>. i)
  let calcL l acc
        | l .&. 1 == 1 =      mappend acc <$> VUM.unsafeRead tree l
        | otherwise = return acc
      calcR r acc
        | r .&. 1 == 1 = flip mappend acc <$> VUM.unsafeRead tree (r - 1)
        | otherwise = return acc
  fix (\loop !accL !accR !l' !r' -> do
    if l' < r'
      then do
        !accL' <- calcL l' accL
        !accR' <- calcR r' accR
        loop accL' accR'
          ((l' + l' .&. 1) .>>>. 1)
          ((r' - r' .&. 1) .>>>. 1)
      else return $! accL <> accR
      ) mempty mempty l r
{-# INLINE mappendFromTo #-}

mappendAll :: (Monoid a, VU.Unbox a) => LazySegmentTree a f -> IO a
mappendAll (LazySegmentTree tree _) = VUM.unsafeRead tree 1
{-# INLINE mappendAll #-}

appAt :: (Monoid a, VU.Unbox a, Monoid f, VU.Unbox f, MonoidAction f a) => LazySegmentTree a f -> Int -> f -> IO ()
appAt st@(LazySegmentTree tree lazy) k0 f = do
  let !n = VUM.length lazy
      k  = k0 + n
      !h = 64 - clz n
  flip VFSM.mapM_ (streamR 1 h) $ \i -> pushLazySegmentTree st (k .>>. i)
  VUM.unsafeModify tree (appMonoid f) k
  flip VFSM.mapM_ (stream 1 h)  $ \i -> updateLazySegmentTree st (k .>>. i)
{-# INLINE appAt #-}

appFromTo :: (Monoid a, VU.Unbox a, Monoid f, VU.Unbox f, MonoidAction f a) => LazySegmentTree a f -> Int -> Int -> f -> IO ()
appFromTo st@(LazySegmentTree _ lazy) l0 r0 f = when (l0 < r0) $ do
  let !n = VUM.length lazy
      !l = l0 + n
      !r = r0 + n
      !h = 64 - clz n
  flip VFSM.mapM_ (streamR 1 h) $ \i -> do
    when ((l .>>. i) .<<. i /= l) $ pushLazySegmentTree st (l .>>>. i)
    when ((r .>>. i) .<<. i /= r) $ pushLazySegmentTree st ((r - 1) .>>>. i)
  fix (\loop !l' !r' -> when (l' < r') $ do
    when (l' .&. 1 == 1) $ appAllAt st l' f
    when (r' .&. 1 == 1) $ appAllAt st (r' - 1) f
    loop ((l' + l' .&. 1) .>>>. 1) ((r' - r' .&. 1) .>>>. 1)
    ) l r
  flip VFSM.mapM_ (stream 1 h) $ \i -> do
    when ((l .>>. i) .<<. i /= l) $ updateLazySegmentTree st (l .>>>. i)
    when ((r .>>. i) .<<. i /= r) $ updateLazySegmentTree st ((r - 1) .>>>. i)
{-# INLINE appFromTo #-}

instance MonoidAction (Sum Int) (Min Int) where
  appMonoid = coerce ((+) @Int)
  {-# INLINE appMonoid #-}


infixl 8 .<<., .>>., .>>>.
infixl 6 .^.

(.<<.) :: Bits b => b -> Int -> b
(.<<.) = unsafeShiftL
{-# INLINE (.<<.) #-}

(.>>.) :: Bits b => b -> Int -> b
(.>>.) = unsafeShiftR
{-# INLINE (.>>.) #-}

(.>>>.) :: Int -> Int -> Int
(.>>>.) (I# x#) (I# i#) = I# (uncheckedIShiftRL# x# i#)
{-# INLINE (.>>>.) #-}

(.^.) :: Bits b => b -> b -> b
(.^.) = xor
{-# INLINE (.^.) #-}

ctz :: FiniteBits fb => fb -> Int
ctz = countTrailingZeros
{-# INLINE ctz #-}

clz :: FiniteBits fb => fb -> Int
clz = countLeadingZeros
{-# INLINE clz #-}

extendToPowerOfTwo :: Int -> Int
extendToPowerOfTwo x
  | x > 1 = (-1) .>>>. clz (x - 1) + 1
  | otherwise = 1

rep :: Monad m => Int -> (Int -> m ()) -> m ()
rep n = flip VFSM.mapM_ (stream 0 n)
{-# INLINE rep #-}

rev :: Monad m => Int -> (Int -> m ()) -> m ()
rev !n = flip VFSM.mapM_ (streamR 0 n)
{-# INLINE rev #-}

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
      | x >= l    = return $ VFSM.Yield x (x - 1)
      | otherwise = return VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] streamR #-}
