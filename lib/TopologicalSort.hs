{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module TopologicalSort where

import           Control.Monad.ST
import           Data.Function
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

topologicalSort :: SparseGraph w -> Maybe (VU.Vector Int)
topologicalSort gr = runST $ do
  let n = numVerticesCSR gr
  q <- newVecQueue n
  let inDegree = VU.unsafeAccumulate (+) (VU.replicate n (0 :: Int)) . VU.map (, 1) $ adjacentCSR gr
  VU.mapM_ (flip enqueueVQ q . fst) . VU.filter ((== 0) . snd) $ VU.indexed inDegree
  inDeg <- VU.unsafeThaw inDegree
  fix $ \loop -> do
    dequeueVQ q >>= \case
      Just v  -> do
        VU.forM_ (gr `adj` v) $ \u -> do
          VUM.unsafeRead inDeg u >>= \case
            1 -> enqueueVQ u q
            i -> VUM.unsafeWrite inDeg u (i - 1)
        loop
      Nothing -> return ()
  enqueueCount <- VUM.unsafeRead (intVarsVQ q) _enqueueCount
  if enqueueCount == n
    then Just <$> VU.unsafeFreeze (internalVQ q)
    else return Nothing

type Vertex     = Int
type Edge       = (Vertex, Vertex)
type EdgeWith w = (Vertex, Vertex, w)
type EdgeId     = Int
data SparseGraph w = CSR
  { numVerticesCSR :: !Int
  , numEdgesCSR    :: !Int
  , offsetCSR      :: !(VU.Vector Int)
  , adjacentCSR    :: !(VU.Vector Vertex)
  , edgeCtxCSR     :: !(VU.Vector w)
  }

data SparseGraphBuilder s w = SparseGraphBuilder
  { numVerticesSGB :: !Int
  , queueSGB       :: VecQueue s (EdgeWith w)
  , outDegSGB      :: VUM.MVector s Int
  }

buildSparseGraph :: VU.Unbox w => Int -> (forall s. SparseGraphBuilder s w -> ST s ()) -> SparseGraph w
buildSparseGraph numVerticesCSR run = runST $ do
  queueSGB    <- newVecQueue (1024 * 1024)
  outDegSGB   <- VUM.replicate numVerticesCSR 0
  run SparseGraphBuilder { numVerticesSGB = numVerticesCSR, .. }
  numEdgesCSR <- lengthVQ queueSGB
  offsetCSR   <- VU.scanl' (+) 0 <$> VU.unsafeFreeze outDegSGB
  moffset     <- VU.thaw offsetCSR
  madj        <- VUM.unsafeNew numEdgesCSR
  mectx       <- VUM.unsafeNew numEdgesCSR
  edges       <- freezeVecQueue queueSGB
  VU.forM_ edges $ \(src, dst, w) -> do
    pos       <- VUM.unsafeRead moffset src
    VUM.unsafeWrite moffset src (pos + 1)
    VUM.unsafeWrite madj pos dst
    VUM.unsafeWrite mectx pos w
  adjacentCSR <- VU.unsafeFreeze madj
  edgeCtxCSR  <- VU.unsafeFreeze mectx
  return CSR{..}
{-# INLINE buildSparseGraph #-}

addDirectedEdge :: VU.Unbox w => SparseGraphBuilder s w -> EdgeWith w -> ST s ()
addDirectedEdge SparseGraphBuilder{..} (src, dst, w) = do
  enqueueVQ (src, dst, w) queueSGB
  VUM.unsafeModify outDegSGB succ src
{-# INLINE addDirectedEdge #-}

addUndirectedEdge :: VU.Unbox w => SparseGraphBuilder s w -> EdgeWith w -> ST s ()
addUndirectedEdge SparseGraphBuilder{..} (src, dst, w) = do
  enqueueVQ (src, dst, w) queueSGB
  enqueueVQ (dst, src, w) queueSGB
  VUM.unsafeModify outDegSGB succ src
  VUM.unsafeModify outDegSGB succ dst
{-# INLINE addUndirectedEdge #-}

addDirectedEdge_ :: SparseGraphBuilder s () -> Edge -> ST s ()
addDirectedEdge_ SparseGraphBuilder{..} (src, dst) = do
  enqueueVQ (src, dst, ()) queueSGB
  VUM.unsafeModify outDegSGB succ src
{-# INLINE addDirectedEdge_ #-}

addUndirectedEdge_ :: SparseGraphBuilder s () -> Edge -> ST s ()
addUndirectedEdge_ SparseGraphBuilder{..} (src, dst) = do
  enqueueVQ (src, dst, ()) queueSGB
  enqueueVQ (dst, src, ()) queueSGB
  VUM.unsafeModify outDegSGB succ src
  VUM.unsafeModify outDegSGB succ dst
{-# INLINE addUndirectedEdge_ #-}

buildDirectedGraph :: Int -> VU.Vector Edge -> SparseGraph ()
buildDirectedGraph numVerticesCSR edges
  = buildSparseGraph numVerticesCSR $ \builder -> do
    VU.mapM_ (addDirectedEdge_ builder) edges

buildUndirectedGraph :: Int -> VU.Vector Edge -> SparseGraph ()
buildUndirectedGraph numVerticesCSR edges
  = buildSparseGraph numVerticesCSR $ \builder -> do
    VU.mapM_ (addUndirectedEdge_ builder) edges

buildDirectedGraphW :: VU.Unbox w => Int -> VU.Vector (EdgeWith w) -> SparseGraph w
buildDirectedGraphW numVerticesCSR edges
  = buildSparseGraph numVerticesCSR $ \builder -> do
    VU.mapM_ (addDirectedEdge builder) edges

buildUndirectedGraphW :: VU.Unbox w => Int -> VU.Vector (EdgeWith w) -> SparseGraph w
buildUndirectedGraphW numVerticesCSR edges
  = buildSparseGraph numVerticesCSR $ \builder -> do
    VU.mapM_ (addUndirectedEdge builder) edges

adj :: SparseGraph w -> Vertex -> VU.Vector Vertex
adj CSR{..} v = VU.unsafeSlice o (o' - o) adjacentCSR
  where
    o  = VU.unsafeIndex offsetCSR v
    o' = VU.unsafeIndex offsetCSR (v + 1)
{-# INLINE adj #-}

iadj :: SparseGraph w -> Vertex -> VU.Vector (EdgeId, Vertex)
iadj CSR{..} v = VU.imap ((,) . (+o)) $ VU.unsafeSlice o (o' - o) adjacentCSR
  where
    o  = VU.unsafeIndex offsetCSR v
    o' = VU.unsafeIndex offsetCSR (v + 1)
{-# INLINE iadj #-}

adjW :: VU.Unbox w => SparseGraph w -> Vertex -> VU.Vector (Vertex, w)
adjW CSR{..} v = VU.zip
  (VU.unsafeSlice o (o' - o) adjacentCSR)
  (VU.unsafeSlice o (o' - o) edgeCtxCSR)
    where
      o  = VU.unsafeIndex offsetCSR v
      o' = VU.unsafeIndex offsetCSR (v + 1)
{-# INLINE adjW #-}

iadjW :: VU.Unbox w => SparseGraph w -> Vertex -> VU.Vector (EdgeId, Vertex, w)
iadjW CSR{..} v = VU.izipWith (\i u w -> (i + o, u, w))
  (VU.unsafeSlice o (o' - o) adjacentCSR)
  (VU.unsafeSlice o (o' - o) edgeCtxCSR)
    where
      o  = VU.unsafeIndex offsetCSR v
      o' = VU.unsafeIndex offsetCSR (v + 1)
{-# INLINE iadjW #-}

outEdges :: SparseGraph w -> Vertex -> VU.Vector EdgeId
outEdges CSR{..} v = VU.generate (o' - o) (+ o)
  where
    o  = VU.unsafeIndex offsetCSR v
    o' = VU.unsafeIndex offsetCSR (v + 1)
{-# INLINE outEdges #-}

outDegree :: SparseGraph w -> Vertex -> Int
outDegree CSR{..} v = VU.unsafeIndex offsetCSR (v + 1) - VU.unsafeIndex offsetCSR v
{-# INLINE outDegree #-}

outDegrees :: SparseGraph w -> VU.Vector Int
outDegrees CSR{..} = VU.zipWith (-) offsetCSR $ VU.tail offsetCSR
{-# INLINE outDegrees #-}

data VecStack s a = VecStack
  { intVarsVS  :: !(VUM.MVector s Int)
  , internalVS :: !(VUM.MVector s a)
  }

_sizeVS :: Int
_sizeVS = 0
{-# INLINE _sizeVS #-}

newVecStack :: VUM.Unbox a => Int -> ST s (VecStack s a)
newVecStack n = VecStack <$> VUM.replicate 1 0 <*> VUM.unsafeNew n

defaultVecStackSize :: Int
defaultVecStackSize = 1024 * 1024

popVS :: VUM.Unbox a => VecStack s a -> ST s (Maybe a)
popVS (VecStack info s) = do
  len <- VUM.unsafeRead info _sizeVS
  if len > 0
    then do
      VUM.unsafeWrite info _sizeVS (len - 1)
      pure <$> VUM.unsafeRead s (len - 1)
    else return Nothing
{-# INLINE popVS #-}

pushVS :: VUM.Unbox a => a -> VecStack s a -> ST s ()
pushVS x (VecStack info s) = do
  len <- VUM.unsafeRead info _sizeVS
  VUM.unsafeWrite s len x
  VUM.unsafeWrite info _sizeVS (len + 1)
{-# INLINE pushVS #-}

pushesVS :: VUM.Unbox a => VU.Vector a -> VecStack s a -> ST s ()
pushesVS vec (VecStack info s) = do
  len <- VUM.unsafeRead info _sizeVS
  VUM.unsafeWrite info _sizeVS (len + VU.length vec)
  VU.unsafeCopy (VUM.unsafeSlice len (VU.length vec) s) vec
{-# INLINE pushesVS #-}

freezeVecStack :: VU.Unbox a => VecStack s a -> ST s (VU.Vector a)
freezeVecStack (VecStack info s) = do
  l <- VUM.unsafeRead info _sizeVS
  VU.unsafeFreeze $ VUM.take l s
{-# INLINE freezeVecStack #-}

data VecQueue s a = VecQueue
  { intVarsVQ  :: !(VUM.STVector s Int)
  , internalVQ :: !(VUM.STVector s a)
  }

_dequeueCount :: Int
_dequeueCount = 0
{-# INLINE _dequeueCount #-}

_enqueueCount :: Int
_enqueueCount = 1
{-# INLINE _enqueueCount #-}

newVecQueue :: VUM.Unbox a => Int -> ST s (VecQueue s a)
newVecQueue n = VecQueue <$> VUM.replicate 2 0 <*> VUM.unsafeNew n

defaultVecQueueSize :: Int
defaultVecQueueSize = 1024 * 1024

lengthVQ :: VUM.Unbox a => VecQueue s a -> ST s Int
lengthVQ (VecQueue info _) = (-)
  <$> VUM.unsafeRead info _enqueueCount
  <*> VUM.unsafeRead info _dequeueCount
{-# INLINE lengthVQ #-}

dequeueVQ :: VUM.Unbox a => VecQueue s a -> ST s (Maybe a)
dequeueVQ (VecQueue info q) = do
  f <- VUM.unsafeRead info _dequeueCount
  r <- VUM.unsafeRead info _enqueueCount
  if f < r
    then do
      VUM.unsafeWrite info _dequeueCount (f + 1)
      pure <$> VUM.unsafeRead q f
    else return Nothing
{-# INLINE dequeueVQ #-}

enqueueVQ :: VUM.Unbox a => a -> VecQueue s a -> ST s ()
enqueueVQ x (VecQueue info q) = do
  r <- VUM.unsafeRead info _enqueueCount
  VUM.unsafeWrite q r x
  VUM.unsafeWrite info _enqueueCount (r + 1)
{-# INLINE enqueueVQ #-}

enqueuesVQ :: VUM.Unbox a => VU.Vector a -> VecQueue s a -> ST s ()
enqueuesVQ vec (VecQueue info q) = do
  r <- VUM.unsafeRead info _enqueueCount
  VUM.unsafeWrite info _enqueueCount (r + VU.length vec)
  VU.unsafeCopy (VUM.unsafeSlice r (VU.length vec) q) vec
{-# INLINE enqueuesVQ #-}

clearVQ :: VUM.Unbox a => VecQueue s a -> ST s ()
clearVQ (VecQueue info _) = do
  VUM.unsafeWrite info _dequeueCount 0
  VUM.unsafeWrite info _enqueueCount 0

freezeVecQueue :: VUM.Unbox a => VecQueue s a -> ST s (VU.Vector a)
freezeVecQueue (VecQueue info q) = do
  f <- VUM.unsafeRead info _dequeueCount
  r <- VUM.unsafeRead info _enqueueCount
  VU.unsafeFreeze $ VUM.unsafeSlice f (r - f) q
