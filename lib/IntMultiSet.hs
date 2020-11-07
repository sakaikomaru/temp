{-# LANGUAGE TypeFamilies #-}

module IntMultiSet where

import           Data.Coerce
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List          as L
import           GHC.Exts

newtype IntMultiSet = IntMultiSet { getIntMultiSet :: IntMap.IntMap Int }
  deriving Eq

instance Show IntMultiSet where
  show = show . toList

instance IsList IntMultiSet where
  type Item IntMultiSet = Int
  fromList = L.foldl' (flip insertIMS) emptyIMS
  toList = concatMap (\(k, x) -> replicate x k) . IntMap.toList . (coerce :: IntMultiSet -> IntMap.IntMap Int)

emptyIMS :: IntMultiSet
emptyIMS = coerce (IntMap.empty :: IntMap.IntMap Int)

singletonIMS :: Int -> IntMultiSet
singletonIMS x = coerce $ IntMap.singleton x (1 :: Int)

insertIMS :: Int -> IntMultiSet -> IntMultiSet
insertIMS x = coerce . IntMap.insertWith (+) x (1 :: Int) . coerce

deleteIMS :: Int -> IntMultiSet -> IntMultiSet
deleteIMS x = (coerce :: IntMap.IntMap Int -> IntMultiSet) . IntMap.update (\y -> if y > 1 then Just $! y - 1 else Nothing) x . coerce

deleteAllIMS :: Int -> IntMultiSet -> IntMultiSet
deleteAllIMS x = (coerce :: IntMap.IntMap Int -> IntMultiSet) . IntMap.delete x . coerce

memberIMS :: Int -> IntMultiSet -> Bool
memberIMS x = IntMap.member x . (coerce :: IntMultiSet -> IntMap.IntMap Int)

notMemberIMS :: Int -> IntMultiSet -> Bool
notMemberIMS x = not . memberIMS x

countIMS :: Int -> IntMultiSet -> Int
countIMS key = IntMap.findWithDefault 0 key . coerce

lookupLTIMS :: Int -> IntMultiSet -> Maybe Int
lookupLTIMS x = fmap fst . IntMap.lookupLT x . (coerce :: IntMultiSet -> IntMap.IntMap Int)

lookupGTIMS :: Int -> IntMultiSet -> Maybe Int
lookupGTIMS x = fmap fst . IntMap.lookupGT x . (coerce :: IntMultiSet -> IntMap.IntMap Int)

lookupLEIMS :: Int -> IntMultiSet -> Maybe Int
lookupLEIMS x = fmap fst . IntMap.lookupLE x . (coerce :: IntMultiSet -> IntMap.IntMap Int)

lookupGEIMS :: Int -> IntMultiSet -> Maybe Int
lookupGEIMS x = fmap fst . IntMap.lookupGE x . (coerce :: IntMultiSet -> IntMap.IntMap Int)

nullIMS :: IntMultiSet -> Bool
nullIMS = IntMap.null . (coerce :: IntMultiSet -> IntMap.IntMap Int)

sizeIMS :: IntMultiSet -> Int
sizeIMS = IntMap.foldl' (+) 0 . (coerce :: IntMultiSet -> IntMap.IntMap Int)

findMinIMS :: IntMultiSet -> Int
findMinIMS = fst . IntMap.findMin . (coerce :: IntMultiSet -> IntMap.IntMap Int)

findMaxIMS :: IntMultiSet -> Int
findMaxIMS = fst . IntMap.findMax . (coerce :: IntMultiSet -> IntMap.IntMap Int)

deleteMinIMS :: IntMultiSet -> IntMultiSet
deleteMinIMS = coerce . IntMap.updateMin (\x -> if x > 1 then Just $! x - 1 else Nothing) . (coerce :: IntMultiSet -> IntMap.IntMap Int)

deleteMaxIMS :: IntMultiSet -> IntMultiSet
deleteMaxIMS = coerce . IntMap.updateMax (\x -> if x > 1 then Just $! x - 1 else Nothing) . (coerce :: IntMultiSet -> IntMap.IntMap Int)

maxViewIMS :: IntMultiSet -> Maybe (Int, IntMultiSet)
maxViewIMS = maybe Nothing just . IntMap.maxViewWithKey . (coerce :: IntMultiSet -> IntMap.IntMap Int)
  where
    just :: ((Int, Int), IntMap.IntMap Int) -> Maybe (Int, IntMultiSet)
    just ((k, x), m)
      | x > 1 = case IntMap.insert k (x - 1) m of
        m' -> Just (k, coerce m')
      | otherwise = Just (k, coerce m)

minViewIMS :: IntMultiSet -> Maybe (Int, IntMultiSet)
minViewIMS = maybe Nothing just . IntMap.minViewWithKey . (coerce :: IntMultiSet -> IntMap.IntMap Int)
  where
    just :: ((Int, Int), IntMap.IntMap Int) -> Maybe (Int, IntMultiSet)
    just ((k, x), m)
      | x > 1 = case IntMap.insert k (x - 1) m of
        m' -> Just (k, coerce m')
      | otherwise = Just (k, coerce m)
