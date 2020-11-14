{-# LANGUAGE BangPatterns #-}

module IntroSort where

import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.ST
import           Data.Bits
import           Data.Word
import           Unsafe.Coerce
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM 
import qualified Data.Vector.Generic               as VG
import qualified Data.Vector.Generic.Mutable       as VGM

introSort :: (Ord a, VG.Vector v a) => v a -> v a
introSort = introSortBy compare

introSortBy :: VG.Vector v a => (a -> a -> Ordering) -> v a -> v a
introSortBy cmp = VG.modify $ inplaceIntroSortBy cmp

inplaceIntroSortBy :: VGM.MVector mv a => (a -> a -> Ordering) -> mv s a -> ST s ()
inplaceIntroSortBy cmp vec = do
  let depthLimit = 2 * floorLog2 (VGM.length vec)
      threshold  = 16
  fix `flip` depthLimit `flip` vec $ \loop !depth mv ->
    when (VGM.length mv > threshold) $
      if depth > 0
        then do
          pivot <- getMedian3Pivot cmp mv
          cut   <- pivotPartition  cmp mv pivot
          loop (depth - 1) (VGM.unsafeDrop cut mv)
          loop (depth - 1) (VGM.unsafeTake cut mv)
        else inplaceHeapSortBy cmp mv
  inplaceInsertionSortBy cmp vec
  where
    floorLog2 :: Int -> Int
    floorLog2 x = fromIntegral $ y .>>. 52 - 1023
      where
        y :: Word64
        y = unsafeCoerce (fromIntegral x :: Double)

pivotPartition :: (VGM.MVector mv a) => (a -> a -> Ordering) -> mv s a -> a -> ST s Int
pivotPartition cmp vec pivot = fix `flip` 0 `flip` VGM.length vec $ \loop !l !r -> do
  !l' <- flip fix l $ \loopL !i -> do
    x   <- VGM.unsafeRead vec i
    case cmp x pivot of
      LT -> loopL (i + 1)
      _  -> return i
  !r' <- flip fix (r - 1) $ \loopR !i -> do
    x <- VGM.unsafeRead vec i
    case cmp pivot x of
      LT -> loopR (i - 1)
      _  -> return i
  if l' < r'
    then do
      VGM.unsafeSwap vec l' r'
      loop (l' + 1) r'
    else return l'
{-# INLINE pivotPartition #-}

getMedian3Pivot :: VGM.MVector mv a => (a -> a -> Ordering) -> mv s a -> ST s a
getMedian3Pivot cmp vec = median cmp <$> VGM.unsafeRead vec 0 <*> VGM.unsafeRead vec (VGM.length vec `quot` 2) <*> VGM.unsafeRead vec (VGM.length vec - 1)
{-# INLINE getMedian3Pivot #-}

median :: (a -> a -> Ordering) -> a -> a -> a -> a
median cmp x y z = case cmp x y of
  LT -> case cmp y z of
    LT -> y
    _  -> case cmp x z of
      LT -> z
      _  -> x
  _  -> case cmp x z of
    LT -> x
    _  -> case cmp y z of
      LT -> z
      _  -> y
{-# INLINE median #-}

inplaceInsertionSortBy :: VGM.MVector mv a => (a -> a -> Ordering) -> mv s a -> ST s ()
inplaceInsertionSortBy cmp vec =
  for 1 (VGM.length vec) $ \i -> do
    x  <- VGM.unsafeRead vec i
    hd <- VGM.unsafeRead vec 0
    case cmp hd x of
      LT -> flip fix i $ \loop !j -> do
        y <- VGM.unsafeRead vec (j - 1)
        case cmp x y of
          LT -> do
            VGM.unsafeWrite vec j y
            loop (j - 1)
          _  -> VGM.unsafeWrite vec j x
      _  -> flip fix i $ \loop !j ->
        if j > 0
          then do
            VGM.unsafeRead vec (j - 1) >>= VGM.unsafeWrite vec j
            loop (j - 1)
          else VGM.unsafeWrite vec 0 x
{-# INLINE inplaceInsertionSortBy #-}

siftDown :: VGM.MVector mv a => (a -> a -> Ordering) -> Int -> mv s a -> ST s ()
siftDown cmp offset vec = do
  let !len = VGM.length vec
  flip fix offset $ \loop !parent -> do
    let !l = 2 * parent + 1
        !r = l + 1
    x <- VGM.unsafeRead vec parent
    when (l < len) $ do
      childL <- VGM.unsafeRead vec l
      if r < len
        then do
          childR <- VGM.unsafeRead vec r
          case cmp childL childR of
            LT -> when (cmp x childR == LT) $ do
              VGM.unsafeSwap vec parent r
              loop r
            _  -> when (cmp x childL == LT) $ do
              VGM.unsafeSwap vec parent l
              loop l
        else when (cmp x childL == LT) $ do
          VGM.unsafeSwap vec parent l
          loop l
{-# INLINE siftDown #-}

heapify :: VGM.MVector mv a => (a -> a -> Ordering) -> mv s a -> ST s ()
heapify cmp vec = rev (VGM.length vec `quot` 2) $ \i -> siftDown cmp i vec
{-# INLINE heapify #-}

inplaceHeapSortBy :: VGM.MVector mv a => (a -> a -> Ordering) -> mv s a -> ST s ()
inplaceHeapSortBy cmp vec = do
  heapify cmp vec
  flip fix (VGM.length vec - 1) $ \loop !i ->
    when (i > 0) $ do
      VGM.unsafeSwap vec 0 i
      siftDown cmp 0 $ VGM.unsafeTake i vec
      loop (i - 1)
{-# INLINE inplaceHeapSortBy #-}

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

for :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
for l r = flip VFSM.mapM_ (stream l r)
{-# INLINE for #-}

rep :: Monad m => Int -> (Int -> m ()) -> m ()
rep = for 0
{-# INLINE rep #-}

rev :: Monad m => Int -> (Int -> m ()) -> m ()
rev n = flip VFSM.mapM_ (streamR 0 n)

infixl 8 .<<., .>>.

(.<<.) :: Bits b => b -> Int -> b
(.<<.) = unsafeShiftL
{-# INLINE (.<<.) #-}

(.>>.) :: Bits b => b -> Int -> b
(.>>.) = unsafeShiftR
{-# INLINE (.>>.) #-}