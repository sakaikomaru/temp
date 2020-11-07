{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}

module PairingHeap where

import           Data.Function
import qualified Data.List     as L
import           GHC.Exts

data MaxHeap a = MaxFork !a [MaxHeap a] | MaxEmpty

emptyMaxPH :: MaxHeap a
emptyMaxPH = MaxEmpty
{-# INLINE emptyMaxPH #-}

singletonMaxPH :: a -> MaxHeap a
singletonMaxPH = flip MaxFork []
{-# INLINE singletonMaxPH #-}

nullMaxPH :: MaxHeap a -> Bool
nullMaxPH (MaxFork _ _) = False
nullMaxPH MaxEmpty      = True
{-# INLINE nullMaxPH #-}

insertMaxPH :: Ord a => a -> MaxHeap a -> MaxHeap a
insertMaxPH = mergeMaxPH . singletonMaxPH
{-# INLINE insertMaxPH #-}

maxElemPH :: MaxHeap a -> Maybe a
maxElemPH (MaxFork x _) = Just x
maxElemPH MaxEmpty      = Nothing
{-# INLINE maxElemPH #-}

deleteMaxPH :: Ord a => MaxHeap a -> Maybe (MaxHeap a)
deleteMaxPH (MaxFork _ hs) = Just $! mergePairsMaxPH hs
deleteMaxPH MaxEmpty       = Nothing
{-# INLINE deleteMaxPH #-}

deleteFindMaxPH :: Ord a => MaxHeap a -> Maybe (a, MaxHeap a)
deleteFindMaxPH (MaxFork x hs) = case mergePairsMaxPH hs of
  merged -> Just (x, merged)
deleteFindMaxPH MaxEmpty       = Nothing
{-# INLINE deleteFindMaxPH #-}

mergeMaxPH :: Ord a => MaxHeap a -> MaxHeap a -> MaxHeap a
mergeMaxPH hx@(MaxFork x hxs) hy@(MaxFork y hys)
  | y <= x    = MaxFork x (hy:hxs)
  | otherwise = MaxFork y (hx:hys)
mergeMaxPH MaxEmpty hy = hy
mergeMaxPH hx MaxEmpty = hx
{-# INLINE mergeMaxPH #-}

mergePairsMaxPH :: Ord a => [MaxHeap a] -> MaxHeap a
mergePairsMaxPH = mconcat . mergePairs
  where
    mergePairs (x:y:xs) = case x <> y of
      merged -> merged : mergePairs xs
    mergePairs xs = xs
{-# INLINE mergePairsMaxPH #-}

instance Ord a => Eq (MaxHeap a) where
  (==) = (==) `on` toList

instance Ord a => Ord (MaxHeap a) where
  compare = compare `on` toList

instance Ord a => IsList (MaxHeap a) where
  type Item (MaxHeap a) = a
  fromList = mergePairsMaxPH . map singletonMaxPH
  toList = L.unfoldr deleteFindMaxPH

instance (Show a, Ord a) => Show (MaxHeap a) where
  show = show . toList

instance Ord a => Semigroup (MaxHeap a) where
  (<>) = mergeMaxPH

instance Ord a => Monoid (MaxHeap a) where
  mempty = emptyMaxPH
  {-# INLINE mempty #-}

data MinHeap a = MinFork !a [MinHeap a] | MinEmpty

emptyMinPH :: MinHeap a
emptyMinPH = MinEmpty
{-# INLINE emptyMinPH #-}

singletonMinPH :: a -> MinHeap a
singletonMinPH = flip MinFork []
{-# INLINE singletonMinPH #-}

nullMinPH :: MinHeap a -> Bool
nullMinPH (MinFork _ _) = False
nullMinPH MinEmpty      = True
{-# INLINE nullMinPH #-}

insertMinPH :: Ord a => a -> MinHeap a -> MinHeap a
insertMinPH = mergeMinPH . singletonMinPH
{-# INLINE insertMinPH #-}

minElemPH :: MinHeap a -> Maybe a
minElemPH (MinFork x _) = Just x
minElemPH MinEmpty      = Nothing
{-# INLINE minElemPH #-}

deleteMinPH :: Ord a => MinHeap a -> Maybe (MinHeap a)
deleteMinPH (MinFork _ hs) = Just $! mergePairsMinPH hs
deleteMinPH MinEmpty       = Nothing
{-# INLINE deleteMinPH #-}

deleteFindMinPH :: Ord a => MinHeap a -> Maybe (a, MinHeap a)
deleteFindMinPH (MinFork x hs) = case mergePairsMinPH hs of
  merged -> Just (x, merged)
deleteFindMinPH MinEmpty       = Nothing
{-# INLINE deleteFindMinPH #-}

mergeMinPH :: Ord a => MinHeap a -> MinHeap a -> MinHeap a
mergeMinPH hx@(MinFork x hxs) hy@(MinFork y hys)
  | x <= y    = MinFork x (hy:hxs)
  | otherwise = MinFork y (hx:hys)
mergeMinPH MinEmpty hy = hy
mergeMinPH hx MinEmpty = hx
{-# INLINE mergeMinPH #-}

mergePairsMinPH :: Ord a => [MinHeap a] -> MinHeap a
mergePairsMinPH = mconcat . mergePairs
  where
    mergePairs (x:y:xs) = case x <> y of
      merged -> merged : mergePairs xs
    mergePairs xs = xs
{-# INLINE mergePairsMinPH #-}

instance Ord a => Eq (MinHeap a) where
  (==) = (==) `on` toList

instance Ord a => Ord (MinHeap a) where
  compare = compare `on` toList

instance Ord a => IsList (MinHeap a) where
  type Item (MinHeap a) = a
  fromList = mergePairsMinPH . map singletonMinPH
  toList = L.unfoldr deleteFindMinPH

instance (Show a, Ord a) => Show (MinHeap a) where
  show = show . toList

instance Ord a => Semigroup (MinHeap a) where
  (<>) = mergeMinPH

instance Ord a => Monoid (MinHeap a) where
  mempty = emptyMinPH
  {-# INLINE mempty #-}