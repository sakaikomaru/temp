module BellmanFord where

import           Control.Monad
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

type Vertex = Int

bellmanFord :: Int -> Vertex -> VU.Vector (Vertex, Vertex, Int) -> VU.Vector Int
bellmanFord n root edges = VU.create $ do
  dist <- VUM.replicate n maxBound
  VUM.write dist root 0
  replicateM_ (n - 1) $ do
    VU.forM_ edges $ \(src, dst, cost) -> do
      dv <- VUM.unsafeRead dist src
      du <- VUM.unsafeRead dist dst
      when (dv + cost < du && dv /= maxBound) $ VUM.unsafeWrite dist dst $ dv + cost
  VU.forM_ edges $ \(src, dst, cost) -> do
    dv <- VUM.unsafeRead dist src
    du <- VUM.unsafeRead dist dst
    when (dv + cost < du && dv /= maxBound) $ VUM.unsafeWrite dist dst minBound
  replicateM_ (n - 1) $ do
    VU.forM_ edges $ \(src, dst, _) -> do
      dv <- VUM.unsafeRead dist src
      when (dv == minBound) $ VUM.unsafeWrite dist dst minBound
  return dist