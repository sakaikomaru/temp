{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module UnionFindST where

import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.ST
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

type UnionFind s = VUM.STVector s Int

newUF :: Int -> ST s (UnionFind s)
newUF n = VUM.replicate n (-1)
{-# INLINE newUF #-}

findUF :: UnionFind s -> Int -> ST s Int
findUF uf x = go x return
  where
    go !x k = do
      px <- VUM.unsafeRead uf x
      if px < 0
        then k x
        else go px $ \ppx -> do
          VUM.unsafeWrite uf x ppx
          k ppx
{-# INLINE findUF #-}

sizeUF :: UnionFind s -> Int -> ST s Int
sizeUF uf = fix $ \loop x -> do
  px <- VUM.unsafeRead uf x
  if px < 0
    then return $! negate px
    else loop px
{-# INLINE sizeUF #-}

uniteUF :: UnionFind s -> Int -> Int -> ST s Bool
uniteUF uf x y = do
  px <- findUF uf x
  py <- findUF uf y
  if px == py
    then return False
    else do
      rx <- VUM.unsafeRead uf px
      ry <- VUM.unsafeRead uf py
      if rx < ry
        then do
          VUM.unsafeModify uf (+ ry) px
          VUM.unsafeWrite uf py px
        else do
          VUM.unsafeModify uf (+ rx) py
          VUM.unsafeWrite uf px py
      return True
{-# INLINE uniteUF #-}

sameUF :: UnionFind s -> Int -> Int -> ST s Bool
sameUF uf x y = (==) `fmap` findUF uf x `ap` findUF uf y
{-# INLINE sameUF #-}

freezeUF :: UnionFind s -> ST s (VU.Vector Int)
freezeUF = VU.unsafeFreeze

countGroupUF :: UnionFind s -> ST s Int
countGroupUF uf = VU.length . VU.filter (< 0) <$> freezeUF uf
{-# INLINE countGroupUF #-}