module Stack where

import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

data Stack a = Stack
  { intVarsStack  :: !(VUM.IOVector Int)
  , internalStack :: !(VUM.IOVector a)
  }

_sizeStack :: Int
_sizeStack = 0
{-# INLINE _sizeStack #-}

newS :: VUM.Unbox a => Int -> IO (Stack a)
newS n = Stack <$> VUM.replicate 1 0 <*> VUM.unsafeNew n

defaultSSize :: Int
defaultSSize = 1024 * 1024

popS :: VUM.Unbox a => Stack a -> IO (Maybe a)
popS (Stack info s) = do
  len <- VUM.unsafeRead info _sizeStack
  if len > 0
    then do
      VUM.unsafeWrite info _sizeStack (len - 1)
      pure <$> VUM.unsafeRead s (len - 1)
    else return Nothing
{-# INLINE popS #-}

pushS :: VUM.Unbox a => Stack a -> a -> IO ()
pushS (Stack info s) x = do
  len <- VUM.unsafeRead info _sizeStack
  VUM.unsafeWrite s len x
  VUM.unsafeWrite info _sizeStack (len + 1)
{-# INLINE pushS #-}

pushesS :: VUM.Unbox a => Stack a -> VU.Vector a -> IO ()
pushesS (Stack info s) vec = do
  len <- VUM.unsafeRead info _sizeStack
  VUM.unsafeWrite info _sizeStack (len + VU.length vec)
  VU.unsafeCopy (VUM.unsafeSlice len (VU.length vec) s) vec
{-# INLINE pushesS #-}

freezeS :: VUM.Unbox a => Stack a -> IO (VU.Vector a)
freezeS (Stack info s) = do
  len <- VUM.unsafeRead info _sizeStack
  VU.unsafeFreeze $ VUM.take len s
{-# INLINE freezeS #-}