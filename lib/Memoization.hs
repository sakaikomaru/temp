{-# LANGUAGE LambdaCase #-}

module Memoization where

import           Control.Monad.State
import           Data.Functor
import qualified Data.Vector                       as V
import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict                   as Map
import           Data.IntMap.Strict                (IntMap)
import qualified Data.IntMap.Strict                as IntMap

memoFix :: Int -> ((Int -> a) -> Int -> a) -> Int -> a
memoFix n f = fix $ \memo -> (V.generate n (f memo) V.!)

memoFixMap :: Ord k => ((k -> State (Map k a) a) -> k -> State (Map k a) a) -> k -> a
memoFixMap f k = flip evalState Map.empty $ do
  flip fix k $ \memo x -> do
    gets (Map.lookup x) >>= \case
      Just fx -> pure fx
      Nothing -> f memo x >>= \fx -> modify' (Map.insert x fx) $> fx

memoFixIntMap :: ((Int -> State (IntMap a) a) -> Int -> State (IntMap a) a) -> Int -> a
memoFixIntMap f n = flip evalState IntMap.empty $ do
  flip fix n $ \memo x -> do
    gets (IntMap.lookup x) >>= \case
      Just fx -> pure fx
      Nothing -> f memo x >>= \fx -> modify' (IntMap.insert x fx) $> fx