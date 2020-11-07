{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TupleSections #-}

module SuffixArray where

import           Data.Bits
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Unsafe      as BSU
import qualified Data.Foldable               as F
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

newtype SuffixArray = SuffixArray { getSuffixArray :: VU.Vector Int }
  deriving Show

getSuffixStartPos :: SuffixArray -> Int -> Int
getSuffixStartPos = VU.unsafeIndex . getSuffixArray
{-# INLINE getSuffixStartPos #-}

getSuffix :: SuffixArray -> Int -> BS.ByteString -> BS.ByteString
getSuffix  = (BSU.unsafeDrop.) . getSuffixStartPos
{-# INLINE getSuffix #-}

buildSuffixArray :: BS.ByteString -> SuffixArray
buildSuffixArray bs = SuffixArray . fst . F.foldl step (sa0, rank0) . takeWhile (<= n) $ map (shiftL 1) [0..]
  where
    !n     = BS.length bs :: Int
    !sa0   = VU.generate (n + 1) id
    !rank0 = VU.generate n (fromIntegral . BSU.unsafeIndex bs) `VU.snoc` 0
    step (!sa, !rank) k = (sa', rank')
      where
        encode sai =
          let !x = rank VU.! sai
              !y
                | sai + k <= n = rank VU.! (sai + k)
                | otherwise    = 0
          in  (x .<<. 40) .|. (y .<<. 20) .|. sai
        maskSA x   = x .&. 0xfffff
        maskRank x = x .>>. 20
        sorted = radixSort64 $ VU.map encode sa
        !sa' = VU.map maskSA sorted
        !rank' = VU.create $ do
          mv <- VUM.unsafeNew (n + 1)
          VUM.write mv (sa' VU.! 0) 1
          VU.forM_ (VU.zip sorted $ VU.tail sorted) $ \(prev, cur) -> do
            x <- VUM.unsafeRead mv (maskSA prev)
            VUM.unsafeWrite mv (maskSA cur) $ x + fromEnum (maskRank prev < maskRank cur)
          return mv

radixSort64 :: VU.Vector Int -> VU.Vector Int
radixSort64 v = F.foldl' step v [0, 16, 32, 48]
  where
    mask k x = fromIntegral $ unsafeShiftR x k .&. 0xffff
    step v k = VU.create $ do
      pref <- VU.unsafeThaw . VU.prescanl' (+) 0 . VU.unsafeAccumulate (+) (VU.replicate 0x10000 0) $ VU.map ((, 1) . mask k) v
      res  <- VUM.unsafeNew $ VU.length v
      VU.forM_ v $ \x -> do
        let !masked = mask k x
        i <- VUM.unsafeRead pref masked
        VUM.unsafeWrite pref masked $ i + 1
        VUM.unsafeWrite res i x
      return res
{-# INLINE radixSort64 #-}

infixl 8 .>>., .<<.

(.>>.) :: Bits b => b -> Int -> b
(.>>.) = unsafeShiftR
{-# INLINE (.>>.) #-}

(.<<.) :: Bits b => b -> Int -> b
(.<<.) = unsafeShiftL
{-# INLINE (.<<.) #-}