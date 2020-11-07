{-# LANGUAGE BangPatterns #-}

module TimSort where

import           Control.Monad
import           Control.Monad.ST
import           Data.Bits
import qualified Data.Vector.Generic.Mutable as VGM

timSort :: (VGM.MVector v a, Ord a) => v s a -> ST s ()
timSort = timSortBy compare
{-# INLINABLE timSort #-}

timSortBy :: VGM.MVector v a => (a -> a -> Ordering) -> v s a -> ST s ()
timSortBy cmp vec
  | mr == len = iter [0] 0 (error "no merge buffer needed!")
  | otherwise = VGM.unsafeNew 256 >>= iter [] 0
  where
    len = VGM.length vec
    mr  = minrun len
    iter s i tmpBuf
      | i >= len  = performRemainingMerges s tmpBuf
      | otherwise = do
        (order, runLen) <- nextRun cmp vec i len
        when (order == Descending) $ VGM.reverse $ VGM.unsafeSlice i runLen vec
        let runEnd = min len (i + max runLen mr)
        sortByBounds' cmp vec i (i + runLen) runEnd
        (s', tmpBuf') <- performMerges (i : s) runEnd tmpBuf
        iter s' runEnd tmpBuf'
    runLengthInvariantBroken a b c i = (b - a <= i - b) || (c - b <= i - c)
    performMerges [b,a] i tmpBuf
      | i - b >= b - a = merge cmp vec a b i tmpBuf >>= performMerges [a] i
    performMerges (c:b:a:ss) i tmpBuf
      | runLengthInvariantBroken a b c i =
        if i - c <= b - a
          then merge cmp vec b c i tmpBuf >>= performMerges (b:a:ss) i
          else do tmpBuf' <- merge cmp vec a b c tmpBuf
                  (ass', tmpBuf'') <- performMerges (a:ss) c tmpBuf'
                  performMerges (c:ass') i tmpBuf''
    performMerges s _ tmpBuf = return (s, tmpBuf)
    performRemainingMerges (b:a:ss) tmpBuf =
      merge cmp vec a b len tmpBuf >>= performRemainingMerges (a:ss)
    performRemainingMerges _ _ = return ()
{-# INLINE timSortBy #-}

sortByBounds :: VGM.MVector v a => (a -> a -> Ordering) -> v s a -> Int -> Int -> ST s ()
sortByBounds cmp a l u
  | len < 2   = return ()
  | len == 2  = sort2ByOffset cmp a l
  | len == 3  = sort3ByOffset cmp a l
  | len == 4  = sort4ByOffset cmp a l
  | otherwise = sort4ByOffset cmp a l >> sortByBounds' cmp a l (l + 4) u
  where
    !len = u - l
{-# INLINE sortByBounds #-}

sortByBounds' :: VGM.MVector v a => (a -> a -> Ordering) -> v s a -> Int -> Int -> Int -> ST s ()
sortByBounds' cmp a l m u = _sort m
  where
    _sort i
      | i < u     = do
        v <- VGM.unsafeRead a i
        insert cmp a l v i
        _sort (i + 1)
      | otherwise = return ()
{-# INLINE sortByBounds' #-}

insert :: VGM.MVector v a => (a -> a -> Ordering) -> v s a -> Int -> a -> Int -> ST s ()
insert cmp a l = loop
  where
    loop val j
      | j <= l    = VGM.unsafeWrite a l val
      | otherwise = do
        e <- VGM.unsafeRead a (j - 1)
        case cmp val e of
          LT -> VGM.unsafeWrite a j e >> loop val (j - 1)
          _  -> VGM.unsafeWrite a j val
{-# INLINE insert #-}

sort2ByOffset :: VGM.MVector v a => (a -> a -> Ordering) -> v s a -> Int -> ST s ()
sort2ByOffset cmp a off = sort2ByIndex cmp a off (off + 1)
{-# INLINABLE sort2ByOffset #-}

sort2ByIndex :: VGM.MVector v a => (a -> a -> Ordering) -> v s a -> Int -> Int -> ST s ()
sort2ByIndex cmp a i j = do
  a0 <- VGM.unsafeRead a i
  a1 <- VGM.unsafeRead a j
  case cmp a0 a1 of
    GT -> VGM.unsafeWrite a i a1 >> VGM.unsafeWrite a j a0
    _  -> return ()
{-# INLINABLE sort2ByIndex #-}

sort3ByOffset :: VGM.MVector v a => (a -> a -> Ordering) -> v s a -> Int -> ST s ()
sort3ByOffset cmp a off = sort3ByIndex cmp a off (off + 1) (off + 2)
{-# INLINABLE sort3ByOffset #-}

sort3ByIndex :: VGM.MVector v a => (a -> a -> Ordering) -> v s a -> Int -> Int -> Int -> ST s ()
sort3ByIndex cmp a i j k = do
  a0 <- VGM.unsafeRead a i
  a1 <- VGM.unsafeRead a j
  a2 <- VGM.unsafeRead a k
  case cmp a0 a1 of
    GT -> case cmp a0 a2 of
      GT -> case cmp a2 a1 of
        LT -> do
          VGM.unsafeWrite a i a2
          VGM.unsafeWrite a k a0
        _  -> do
          VGM.unsafeWrite a i a1
          VGM.unsafeWrite a j a2
          VGM.unsafeWrite a k a0
      _  -> do
        VGM.unsafeWrite a i a1
        VGM.unsafeWrite a j a0
    _  -> case cmp a1 a2 of
      GT -> case cmp a0 a2 of
        GT -> do
          VGM.unsafeWrite a i a2
          VGM.unsafeWrite a j a0
          VGM.unsafeWrite a k a1
        _  -> do
          VGM.unsafeWrite a j a2
          VGM.unsafeWrite a k a1
      _  -> return ()
{-# INLINABLE sort3ByIndex #-}

sort4ByOffset :: VGM.MVector v a => (a -> a -> Ordering) -> v s a -> Int -> ST s ()
sort4ByOffset cmp a off = sort4ByIndex cmp a off (off + 1) (off + 2) (off + 3)
{-# INLINABLE sort4ByOffset #-}

sort4ByIndex :: VGM.MVector v a => (a -> a -> Ordering) -> v s a -> Int -> Int -> Int -> Int -> ST s ()
sort4ByIndex cmp a i j k l = do
  a0 <- VGM.unsafeRead a i
  a1 <- VGM.unsafeRead a j
  a2 <- VGM.unsafeRead a k
  a3 <- VGM.unsafeRead a l
  case cmp a0 a1 of
    GT -> case cmp a0 a2 of
      GT -> case cmp a1 a2 of
        GT -> case cmp a1 a3 of
          GT -> case cmp a2 a3 of
            GT -> do
              VGM.unsafeWrite a i a3
              VGM.unsafeWrite a j a2
              VGM.unsafeWrite a k a1
              VGM.unsafeWrite a l a0
            _  -> do
              VGM.unsafeWrite a i a2
              VGM.unsafeWrite a j a3
              VGM.unsafeWrite a k a1
              VGM.unsafeWrite a l a0
          _  -> case cmp a0 a3 of
            GT -> do
              VGM.unsafeWrite a i a2
              VGM.unsafeWrite a j a1
              VGM.unsafeWrite a k a3
              VGM.unsafeWrite a l a0
            _  -> do
              VGM.unsafeWrite a i a2
              VGM.unsafeWrite a j a1
              VGM.unsafeWrite a k a0
              VGM.unsafeWrite a l a3
        _ -> case cmp a2 a3 of
          GT -> case cmp a1 a3 of
            GT -> do
              VGM.unsafeWrite a i a3
              VGM.unsafeWrite a j a1
              VGM.unsafeWrite a k a2
              VGM.unsafeWrite a l a0
            _  -> do
              VGM.unsafeWrite a i a1
              VGM.unsafeWrite a j a3
              VGM.unsafeWrite a k a2
              VGM.unsafeWrite a l a0
          _  -> case cmp a0 a3 of
            GT -> do
              VGM.unsafeWrite a i a1
              VGM.unsafeWrite a j a2
              VGM.unsafeWrite a k a3
              VGM.unsafeWrite a l a0
            _  -> do
              VGM.unsafeWrite a i a1
              VGM.unsafeWrite a j a2
              VGM.unsafeWrite a k a0
              -- VGM.unsafeWrite a l a3
      _  -> case cmp a0 a3 of
        GT -> case cmp a1 a3 of
          GT -> do
            VGM.unsafeWrite a i a3
            -- VGM.unsafeWrite a j a1
            VGM.unsafeWrite a k a0
            VGM.unsafeWrite a l a2
          _  -> do
            VGM.unsafeWrite a i a1
            VGM.unsafeWrite a j a3
            VGM.unsafeWrite a k a0
            VGM.unsafeWrite a l a2
        _  -> case cmp a2 a3 of
          GT -> do
            VGM.unsafeWrite a i a1
            VGM.unsafeWrite a j a0
            VGM.unsafeWrite a k a3
            VGM.unsafeWrite a l a2
          _  -> do
            VGM.unsafeWrite a i a1
            VGM.unsafeWrite a j a0
            -- VGM.unsafeWrite a k a2
            -- VGM.unsafeWrite a l a3
    _  -> case cmp a1 a2 of
      GT -> case cmp a0 a2 of
        GT -> case cmp a0 a3 of
          GT -> case cmp a2 a3 of
            GT -> do
              VGM.unsafeWrite a i a3
              VGM.unsafeWrite a j a2
              VGM.unsafeWrite a k a0
              VGM.unsafeWrite a l a1
            _  -> do
              VGM.unsafeWrite a i a2
              VGM.unsafeWrite a j a3
              VGM.unsafeWrite a k a0
              VGM.unsafeWrite a l a1
          _  -> case cmp a1 a3 of
            GT -> do
              VGM.unsafeWrite a i a2
              VGM.unsafeWrite a j a0
              VGM.unsafeWrite a k a3
              VGM.unsafeWrite a l a1
            _  -> do
              VGM.unsafeWrite a i a2
              VGM.unsafeWrite a j a0
              VGM.unsafeWrite a k a1
              -- VGM.unsafeWrite a l a3
        _  -> case cmp a2 a3 of
          GT -> case cmp a0 a3 of
            GT -> do
              VGM.unsafeWrite a i a3
              VGM.unsafeWrite a j a0
              -- VGM.unsafeWrite a k a2
              VGM.unsafeWrite a l a1
            _  -> do
              -- VGM.unsafeWrite a i a0
              VGM.unsafeWrite a j a3
              -- VGM.unsafeWrite a k a2
              VGM.unsafeWrite a l a1
          _  -> case cmp a1 a3 of
            GT -> do
              -- VGM.unsafeWrite a i a0
              VGM.unsafeWrite a j a2
              VGM.unsafeWrite a k a3
              VGM.unsafeWrite a l a1
            _  -> do
              -- VGM.unsafeWrite a i a0
              VGM.unsafeWrite a j a2
              VGM.unsafeWrite a k a1
              -- VGM.unsafeWrite a l a3
      _  -> case cmp a1 a3 of
        GT -> case cmp a0 a3 of
          GT -> do
            VGM.unsafeWrite a i a3
            VGM.unsafeWrite a j a0
            VGM.unsafeWrite a k a1
            VGM.unsafeWrite a l a2
          _  -> do
            -- VGM.unsafeWrite a i a0
            VGM.unsafeWrite a j a3
            VGM.unsafeWrite a k a1
            VGM.unsafeWrite a l a2
        _  -> case cmp a2 a3 of
          GT -> do
            -- VGM.unsafeWrite a i a0
            -- VGM.unsafeWrite a j a1
            VGM.unsafeWrite a k a3
            VGM.unsafeWrite a l a2
          _  -> do
            -- VGM.unsafeWrite a i a0
            -- VGM.unsafeWrite a j a1
            -- VGM.unsafeWrite a k a2
            -- VGM.unsafeWrite a l a3
            return ()
{-# INLINABLE sort4ByIndex #-}


minrun :: Int -> Int
minrun n0 = (n0 .>>. extra) + if (lowMask .&. n0) > 0 then 1 else 0
  where
    !n1 = n0 .|. (n0 .>>. 1)
    !n2 = n1 .|. (n1 .>>. 2)
    !n3 = n2 .|. (n2 .>>. 4)
    !n4 = n3 .|. (n3 .>>. 8)
    !n5 = n4 .|. (n4 .>>. 16)
    !n6 = n5 .|. (n5 .>>. 32)
    !lowMask = n6 .>>. 6
    !extra = popCount lowMask
{-# INLINE minrun #-}

data Order = Ascending | Descending deriving (Eq, Show)

nextRun :: VGM.MVector v a => (a -> a -> Ordering) -> v s a -> Int -> Int -> ST s (Order, Int)
nextRun _ _ i len | i + 1 >= len = return (Ascending, 1)
nextRun cmp vec i len = do
  x <- VGM.unsafeRead vec i
  y <- VGM.unsafeRead vec (i + 1)
  if x `gt` y then desc y 2 else asc  y 2
  where
    gt a b = cmp a b == GT
    desc _ !k | i + k >= len = return (Descending, k)
    desc x !k = do
      y <- VGM.unsafeRead vec (i + k)
      if x `gt` y then desc y (k + 1) else return (Descending, k)
    asc _ !k | i + k >= len = return (Ascending, k)
    asc x !k = do
      y <- VGM.unsafeRead vec (i + k)
      if x `gt` y then return (Ascending, k) else asc y (k + 1)
{-# INLINE nextRun #-}

minGallop :: Int
minGallop = 7
{-# INLINE minGallop #-}

merge :: VGM.MVector v a => (a -> a -> Ordering) -> v s a -> Int -> Int -> Int -> v s a -> ST s (v s a)
merge cmp vec l m u tmpBuf = do
  vm <- VGM.unsafeRead vec m
  l' <- gallopingSearchLeftPBounds (`gt` vm) vec l m
  if l' >= m
    then return tmpBuf
    else do
      vn <- VGM.unsafeRead vec (m - 1)
      u' <- gallopingSearchRightPBounds (`gte` vn) vec m u
      if u' <= m
        then return tmpBuf
        else (if (m - l') <= (u' - m) then mergeLo else mergeHi) cmp vec l' m u' tmpBuf
  where
    gt  a b = cmp a b == GT
    gte a b = cmp a b /= LT
{-# INLINE merge #-}

mergeLo :: VGM.MVector v a => (a -> a -> Ordering) -> v s a -> Int -> Int -> Int -> v s a -> ST s (v s a)
mergeLo cmp vec l m u tempBuf' = do
  tmpBuf <- cloneSlice l tmpBufLen vec tempBuf'
  vi <- VGM.unsafeRead tmpBuf 0
  vj <- VGM.unsafeRead vec m
  iter tmpBuf 0 m l vi vj minGallop minGallop
  return tmpBuf
  where
    gt  a b = cmp a b == GT
    gte a b = cmp a b /= LT
    tmpBufLen = m - l
    finalize tmpBuf i k = do
      let from = VGM.unsafeSlice i (tmpBufLen - i) tmpBuf
          to   = VGM.unsafeSlice k (tmpBufLen - i) vec
      VGM.unsafeCopy to from
    iter _ i _ _ _ _ _ _ | i >= tmpBufLen = return ()
    iter tmpBuf i j k _ _ _ _ | j >= u    = finalize tmpBuf i k
    iter tmpBuf i j k _ vj 0 _ = do
      i' <- gallopingSearchLeftPBounds (`gt` vj) tmpBuf i tmpBufLen
      let gallopLen = i' - i
          from = VGM.unsafeSlice i gallopLen tmpBuf
          to   = VGM.unsafeSlice k gallopLen vec
      VGM.unsafeCopy to from
      when (i' < tmpBufLen) $ do
        vi' <- VGM.unsafeRead tmpBuf i'
        iter tmpBuf i' j (k + gallopLen) vi' vj minGallop minGallop
    iter tmpBuf i j k vi _ _ 0 = do
      j' <- gallopingSearchLeftPBounds (`gte` vi) vec j u
      let gallopLen = j' - j
          from = VGM.slice j gallopLen vec
          to   = VGM.slice k gallopLen vec
      VGM.unsafeMove to from
      if j' >= u then finalize tmpBuf i (k + gallopLen) else do
        vj' <- VGM.unsafeRead vec j'
        iter tmpBuf i j' (k + gallopLen) vi vj' minGallop minGallop
    iter tmpBuf i j k vi vj ga gb
      | vj `gte` vi = do
        VGM.unsafeWrite vec k vi
        when (i + 1 < tmpBufLen) $ do
          vi' <- VGM.unsafeRead tmpBuf (i + 1)
          iter tmpBuf (i + 1) j (k + 1) vi' vj (ga - 1) minGallop
      | otherwise   = do
        VGM.unsafeWrite vec k vj
        if j + 1 >= u
          then finalize tmpBuf i (k + 1)
          else do
            vj' <- VGM.unsafeRead vec (j + 1)
            iter tmpBuf i (j + 1) (k + 1) vi vj' minGallop (gb - 1)
{-# INLINE mergeLo #-}

mergeHi :: VGM.MVector v a => (a -> a -> Ordering) -> v s a -> Int -> Int -> Int -> v s a -> ST s (v s a)
mergeHi cmp vec l m u tmpBuf' = do
  tmpBuf <- cloneSlice m tmpBufLen vec tmpBuf'
  vi <- VGM.unsafeRead vec (m - 1)
  vj <- VGM.unsafeRead tmpBuf (tmpBufLen - 1)
  iter tmpBuf (m - 1) (tmpBufLen - 1) (u - 1) vi vj minGallop minGallop
  return tmpBuf
  where
    gt  a b = cmp a b == GT
    gte a b = cmp a b /= LT
    tmpBufLen = u - m
    finalize tmpBuf j = do
      let from = VGM.unsafeSlice 0 (j + 1) tmpBuf
          to   = VGM.unsafeSlice l (j + 1) vec
      VGM.unsafeCopy to from
    iter _ _ j _ _ _ _ _ | j < 0 = return ()
    iter tmpBuf i j _ _ _ _ _ | i < l = finalize tmpBuf j
    iter tmpBuf i j k _ vj 0 _ = do
      i' <- gallopingSearchRightPBounds (`gt` vj) vec l i
      let gallopLen = i - i'
          from = VGM.slice (i' + 1) gallopLen vec
          to   = VGM.slice (k - gallopLen + 1) gallopLen vec
      VGM.unsafeMove to from
      if i' < l
        then finalize tmpBuf j
        else do
          vi' <- VGM.unsafeRead vec i'
          iter tmpBuf i' j (k - gallopLen) vi' vj minGallop minGallop
    iter tmpBuf i j k vi _ _ 0 = do
      j' <- gallopingSearchRightPBounds (`gte` vi) tmpBuf 0 j
      let gallopLen = j - j'
          from = VGM.slice (j' + 1) gallopLen tmpBuf
          to   = VGM.slice (k - gallopLen + 1) gallopLen vec
      VGM.unsafeCopy to from
      when (j' >= 0) $ do
        vj' <- VGM.unsafeRead tmpBuf j'
        iter tmpBuf i j' (k - gallopLen) vi vj' minGallop minGallop
    iter tmpBuf i j k vi vj ga gb
      | vi `gt` vj = do
        VGM.unsafeWrite vec k vi
        if i - 1 < l
          then finalize tmpBuf j
          else do
            vi' <- VGM.unsafeRead vec (i - 1)
            iter tmpBuf (i - 1) j (k - 1) vi' vj (ga - 1) minGallop
      | otherwise  = do
        VGM.unsafeWrite vec k vj
        when (j - 1 >= 0) $ do
          vj' <- VGM.unsafeRead tmpBuf (j - 1)
          iter tmpBuf i (j - 1) (k - 1) vi vj' minGallop (gb - 1)
{-# INLINE mergeHi #-}

cloneSlice :: VGM.MVector v a => Int -> Int -> v s a -> v s a -> ST s (v s a)
cloneSlice i len vec tmpBuf = do
  tmpBuf' <- ensureCapacity len tmpBuf
  VGM.unsafeCopy (VGM.unsafeSlice 0 len tmpBuf') (VGM.unsafeSlice i len vec)
  return tmpBuf'
{-# INLINE cloneSlice #-}

ensureCapacity :: VGM.MVector v a => Int -> v s a -> ST s (v s a)
ensureCapacity l tmpBuf
  | l <= VGM.length tmpBuf = return tmpBuf
  | otherwise              = VGM.unsafeNew (2 * l)
{-# INLINE ensureCapacity #-}

gallopingSearchLeftPBounds :: VGM.MVector v a => (a -> Bool) -> v s a -> Int -> Int -> ST s Int
gallopingSearchLeftPBounds p vec l u
  | u <= l    = return l
  | otherwise = do
    x <- VGM.unsafeRead vec l
    if p x then return l else iter (l + 1) l 2
  where
    binSearch = binarySearchPBounds p vec
    iter !i !j !_stepSize | i >= u - 1 = do
      x <- VGM.unsafeRead vec (u - 1)
      if p x then binSearch (j + 1) (u - 1) else return u
    iter !i !j !stepSize = do
      x <- VGM.unsafeRead vec i
      if p x then binSearch (j + 1) i else iter (i + stepSize) i (2 * stepSize)
{-# INLINE gallopingSearchLeftPBounds #-}

gallopingSearchRightPBounds :: VGM.MVector v a => (a -> Bool) -> v s a -> Int -> Int -> ST s Int
gallopingSearchRightPBounds p vec l u
  | u <= l    = return l
  | otherwise = iter (u - 1) (u - 1) (-1)
  where
    binSearch = binarySearchPBounds p vec
    iter !i !j !_stepSize | i <= l = do
      x <- VGM.unsafeRead vec l
      if p x then return l else binSearch (l + 1) j
    iter !i !j !stepSize = do
      x <- VGM.unsafeRead vec i
      if p x then iter (i + stepSize) i (2 * stepSize) else binSearch (i + 1) j
{-# INLINE gallopingSearchRightPBounds #-}

binarySearchPBounds :: VGM.MVector v a => (a -> Bool) -> v s a -> Int -> Int -> ST s Int
binarySearchPBounds p vec = loop
  where
    loop !l !u
      | u <= l    = return l
      | otherwise = VGM.unsafeRead vec k >>= \e -> if p e then loop l k else loop (k + 1) u
      where
        k = midPoint u l
{-# INLINE binarySearchPBounds #-}

midPoint :: Int -> Int -> Int
midPoint a b = toInt $ (toWord a + toWord b) `div` 2
  where
    toWord :: Int -> Word
    toWord = fromIntegral
    toInt :: Word -> Int
    toInt = fromIntegral
{-# INLINE midPoint #-}

infixl 8 .<<., .>>.

(.<<.) :: Bits b => b -> Int -> b
(.<<.) = unsafeShiftL
{-# INLINE (.<<.) #-}

(.>>.) :: Bits b => b -> Int -> b
(.>>.) = unsafeShiftR
{-# INLINE (.>>.) #-}