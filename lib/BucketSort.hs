{-# LANGUAGE TupleSections #-}

module BucketSort where

import qualified Data.Vector.Unboxed as VU

-- >>> bucketSort 10 (VU.fromList ([4, 8, 1, 6, 9, 3, 40, 13, 27] :: [Int]))
-- [1,3,4,6,8,9]
-- >>> bucketSort 100 (VU.fromList ([4, 8, 1, 6, 9, 3, 40, 13, 27] :: [Int]))
-- [1,3,4,6,8,9,13,27,40]
bucketSort :: Int -> VU.Vector Int -> VU.Vector Int
bucketSort bucketSize
  = VU.concatMap (uncurry $ flip VU.replicate) . VU.indexed . VU.unsafeAccumulate (+) (VU.replicate bucketSize 0) . VU.map (,1)
{-# INLINE bucketSort #-}
