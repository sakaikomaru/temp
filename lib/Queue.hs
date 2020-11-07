module Queue where

import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

data Queue a = Queue
  { intVarsQueue  :: !(VUM.IOVector Int)
  , internalQueue :: !(VUM.IOVector a)
  }

_dequeueCount :: Int
_dequeueCount = 0
{-# INLINE _dequeueCount #-}

_enqueueCount :: Int
_enqueueCount = 1
{-# INLINE _enqueueCount #-}

newQ  :: VUM.Unbox a => Int -> IO (Queue a)
newQ n = Queue <$> VUM.replicate 2 0 <*> VUM.unsafeNew n

defaultQSize :: Int
defaultQSize = 1024 * 1024

lengthQ :: VUM.Unbox a => Queue a -> IO Int
lengthQ (Queue info _) = (-) <$> VUM.unsafeRead info _enqueueCount <*> VUM.unsafeRead info _dequeueCount
{-# INLINE lengthQ #-}

dequeueQ :: VUM.Unbox a => Queue a -> IO (Maybe a)
dequeueQ (Queue info q) = do
  f <- VUM.unsafeRead info _dequeueCount
  r <- VUM.unsafeRead info _enqueueCount
  if f < r
    then do
      VUM.unsafeWrite info _dequeueCount (f + 1)
      pure <$> VUM.unsafeRead q f
    else return Nothing
{-# INLINE dequeueQ #-}

enqueueQ :: VUM.Unbox a => Queue a -> a -> IO ()
enqueueQ (Queue info q) x = do
  r <- VUM.unsafeRead info _enqueueCount
  VUM.unsafeWrite q r x
  VUM.unsafeWrite info _enqueueCount (r + 1)
{-# INLINE enqueueQ #-}

enqueuesQ :: VUM.Unbox a => Queue a -> VU.Vector a -> IO ()
enqueuesQ (Queue info q) vec = do
  r <- VUM.unsafeRead info _enqueueCount
  VUM.unsafeWrite info _enqueueCount (r + VU.length vec)
  VU.unsafeCopy (VUM.unsafeSlice r (VU.length vec) q) vec
{-# INLINE enqueuesQ #-}

clearQ :: VUM.Unbox a => Queue a -> IO ()
clearQ (Queue info _) = do
  VUM.unsafeWrite info _dequeueCount 0
  VUM.unsafeWrite info _enqueueCount 0

freezeQ :: VUM.Unbox a => Queue a -> IO (VU.Vector a)
freezeQ (Queue info q) = do
  f <- VUM.unsafeRead info _dequeueCount
  r <- VUM.unsafeRead info _enqueueCount
  VU.unsafeFreeze $ VUM.unsafeSlice f (r - f) q