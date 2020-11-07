{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module BinaryHeap where

import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.Fix
import           Data.Bits
import           Data.Coerce
import           Data.Functor.Identity
import           Data.Ord
import           GHC.Exts
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM
import qualified Data.Vector.Unboxed               as VU
import qualified Data.Vector.Unboxed.Mutable       as VUM

data BinaryHeap (f :: * -> *) s a = BinaryHeap
  { priorityBH    :: a -> f a
  , intVarsBH     :: !(VUM.STVector s Int)
  , internalVecBH :: !(VUM.STVector s a)
  }

_sizeBH :: Int
_sizeBH = 0
{-# INLINE _sizeBH #-}

type MinBinaryHeap s a = BinaryHeap Identity s a
type MaxBinaryHeap s a = BinaryHeap Down s a

newBinaryHeap :: VU.Unbox a => (a -> f a) -> Int -> ST s (BinaryHeap f s a)
newBinaryHeap prio n = BinaryHeap prio <$> VUM.replicate 1 0 <*> VUM.unsafeNew n

newMinBH :: VU.Unbox a => Int -> ST s (MinBinaryHeap s a)
newMinBH = newBinaryHeap Identity

newMaxBH :: VU.Unbox a => Int -> ST s (MaxBinaryHeap s a)
newMaxBH = newBinaryHeap Down

getBHSize :: BinaryHeap f s a -> ST s Int
getBHSize BinaryHeap{..} = VUM.unsafeRead intVarsBH _sizeBH
{-# INLINE getBHSize #-}

siftUpBy :: VU.Unbox a => (a -> a -> Ordering) -> VUM.STVector s a -> Int -> ST s ()
siftUpBy cmp vec k = do
  x <- VUM.unsafeRead vec k
  flip fix k $ \loop !i ->
    if i > 0
      then do
        let !parent = (i - 1) .>>. 1
        p <- VUM.unsafeRead vec parent
        case cmp p x of
          GT -> VUM.unsafeWrite vec i p >> loop parent
          _  -> VUM.unsafeWrite vec i x
      else VUM.unsafeWrite vec 0 x
{-# INLINE siftUpBy #-}

siftDownBy :: VU.Unbox a => (a -> a -> Ordering) -> VUM.STVector s a -> Int -> ST s ()
siftDownBy cmp vec k = do
  x <- VUM.unsafeRead vec k
  let !n = VUM.length vec
  flip fix k $ \loop !i -> do
    let l = i .<<. 1 .|. 1
    let r = l + 1
    if n <= l
      then VUM.unsafeWrite vec i x
      else do
        childL <- VUM.unsafeRead vec l
        if r < n
          then do
            childR <- VUM.unsafeRead vec r
            case cmp childR childL of
              LT -> case cmp x childR of
                GT -> VUM.unsafeWrite vec i childR >> loop r
                _  -> VUM.unsafeWrite vec i x
              _  -> case cmp x childL of
                GT -> VUM.unsafeWrite vec i childL >> loop l
                _  -> VUM.unsafeWrite vec i x
          else case cmp x childL of
            GT -> VUM.unsafeWrite vec i childL >> loop l
            _  -> VUM.unsafeWrite vec i x
{-# INLINE siftDownBy #-}

heapifyBy :: VU.Unbox a => (a -> a -> Ordering) -> VUM.STVector s a -> ST s ()
heapifyBy cmp vec = rev (VUM.length vec `quot` 2) $ \i -> siftDownBy cmp vec i
{-# INLINE heapifyBy #-}

class OrdVia f a where
  compareVia :: (a -> f a) -> a -> a -> Ordering

instance Ord a => OrdVia Identity a where
  compareVia _ = coerce (compare :: Identity a -> Identity a -> Ordering)

instance Ord a => OrdVia Down a where
  compareVia _ = coerce (compare :: Down a -> Down a -> Ordering)

buildBinaryHeapVia :: (OrdVia f a, VU.Unbox a) => (a -> f a) -> VU.Vector a -> ST s (BinaryHeap f s a)
buildBinaryHeapVia priorityBH vec = do
  intVarsBH <- VUM.replicate 1 $ VU.length vec
  internalVecBH <- VU.thaw vec
  heapifyBy (compareVia priorityBH) internalVecBH
  return $! BinaryHeap{..}
{-# INLINE buildBinaryHeapVia #-}

buildMinBH :: (Ord a, VU.Unbox a) => VU.Vector a -> ST s (BinaryHeap Identity s a)
buildMinBH = buildBinaryHeapVia Identity
{-# INLINE buildMinBH #-}

buildMaxBH :: (Ord a, VU.Unbox a) => VU.Vector a -> ST s (BinaryHeap Down s a)
buildMaxBH = buildBinaryHeapVia Down
{-# INLINE buildMaxBH #-}

unsafeViewBH :: VU.Unbox a => BinaryHeap f s a -> ST s a
unsafeViewBH BinaryHeap{..} = VUM.unsafeRead internalVecBH 0
{-# INLINE unsafeViewBH #-}

viewBH :: VU.Unbox a => BinaryHeap f s a -> ST s (Maybe a)
viewBH bh = do
  size <- getBHSize bh
  if size > 0
    then Just <$!> unsafeViewBH bh
    else return Nothing
{-# INLINE viewBH #-}

insertBH :: (OrdVia f a, VU.Unbox a) => BinaryHeap f s a -> a -> ST s ()
insertBH BinaryHeap{..} x = do
  size <- VUM.unsafeRead intVarsBH _sizeBH
  VUM.unsafeWrite intVarsBH _sizeBH (size + 1)
  VUM.unsafeWrite internalVecBH size x
  siftUpBy (compareVia priorityBH) internalVecBH size
{-# INLINE insertBH #-}

unsafeDeleteBH :: (OrdVia f a, VU.Unbox a) => BinaryHeap f s a -> ST s ()
unsafeDeleteBH  BinaryHeap{..} = do
  size <- pred <$!> VUM.unsafeRead intVarsBH _sizeBH
  VUM.unsafeWrite intVarsBH _sizeBH size
  VUM.unsafeSwap internalVecBH 0 size
  siftDownBy (compareVia priorityBH) (VUM.unsafeTake size internalVecBH) 0
{-# INLINE unsafeDeleteBH #-}

modifyTopBH :: (OrdVia f a, VU.Unbox a) => BinaryHeap f s a -> (a -> a) -> ST s ()
modifyTopBH BinaryHeap{..} f = do
  VUM.unsafeModify internalVecBH f 0
  size <- VUM.unsafeRead intVarsBH _sizeBH
  siftDownBy (compareVia priorityBH) (VUM.unsafeTake size internalVecBH) 0
{-# INLINE modifyTopBH #-}

deleteFindTopMinBH :: (Ord a, VU.Unbox a) => MinBinaryHeap s a -> ST s (Maybe a)
deleteFindTopMinBH bh = do
  size <- getBHSize bh
  if size > 0
    then do
      !top <- unsafeViewBH bh <* unsafeDeleteBH bh
      return $ Just top
    else return Nothing
{-# INLINE deleteFindTopMinBH #-}

deleteFindTopMaxBH :: (Ord a, VU.Unbox a) => MaxBinaryHeap s a -> ST s (Maybe a)
deleteFindTopMaxBH bh = do
  size <- getBHSize bh
  if size > 0
    then do
      !top <- unsafeViewBH bh <* unsafeDeleteBH bh
      return $ Just top
    else return Nothing
{-# INLINE deleteFindTopMaxBH #-}

clearBH :: BinaryHeap f s a -> ST s ()
clearBH BinaryHeap{..} = VUM.unsafeWrite intVarsBH 0 0

freezeInternalVecBH :: VU.Unbox a => BinaryHeap f s a -> ST s (VU.Vector a)
freezeInternalVecBH BinaryHeap{..} = do
  size <- VUM.unsafeRead intVarsBH _sizeBH
  VU.unsafeFreeze (VUM.unsafeTake size internalVecBH)



streamR :: Monad m => Int -> Int -> VFSM.Stream m Int
streamR !l !r = VFSM.Stream step (r - 1)
  where
    step x
      | x >= l    = return $ VFSM.Yield x (x - 1)
      | otherwise = return $ VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE streamR #-}

rev :: Monad m => Int -> (Int -> m ()) -> m ()
rev n = flip VFSM.mapM_ (streamR 0 n)
{-# INLINE rev #-}

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
(.^.)  = xor
{-# INLINE (.^.)  #-}