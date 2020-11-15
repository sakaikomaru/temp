{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash    #-}

module FastZetaMoebiusTransform where

import           Control.Monad
import           Control.Monad.Cont
import           Control.Monad.ST
import           Data.Bits
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM
import qualified Data.Vector.Unboxed               as VU
import qualified Data.Vector.Unboxed.Mutable       as VUM
import           GHC.Exts

test :: IO ()
test = do
  let
    xs = VU.fromList ([ 1, 0, 1, 0, 0, 1, 1, 0] :: [Int])
    ys = VU.fromList ([ 4, 2, 0,-2, 0, 2, 0, 2] :: [Int])
  print $ fztXOR xs
  print $ fmtXOR ys
  -- print $ fmtGCD ys

{- ゼータ変換とメビウス変換 (bitAND)
A    = { 0, 1, 2 } <- サイズ2^nの配列におけるnがAの大きさ
S    = { { }, {0}, {1}, {2}, {0, 1}, {0, 2}, {1, 2}, {0, 1, 2} }
各部分集合について得点が割り振られているとする
f(S) =    3 ,  1 ,  4 ,  1 ,   5   ,   9   ,    2  ,     6
ここで、部分集合 {1} を含む部分集合Tの得点の総{和}を求めたいとする

TODO: ↑和でない場合の実装も行う

T    = { {1}, {0, 1}, {1, 2}, {0, 1, 2} }
f(T) =    4 +   5   +   2   +  6 = 17
これをSの部分集合すべてについて行う

S          = { { }, {0}, {1}, {2}, {0, 1}, {0, 2}, {1, 2}, {0, 1, 2} }
f(S)       =    3 ,  1 ,  4 ,  1 ,   5   ,   9   ,    2  ,     6
fztAND(S)  =   31, 21 , 17 , 18 ,   11  ,   15  ,    8  ,     6
fmtAND(fztAND(S))
           =    3 ,  1 ,  4 ,  1 ,   5   ,   9   ,    2  ,     6
この逆変換fmtUがメビウス変換
-}

fztAND :: VU.Vector Int -> VU.Vector Int
fztAND vec = VU.create $ do
  let
    !f = growVU vec
    !n = VU.length f
  g <- VU.unsafeThaw f
  forG 1 (n - 1) const 0 unsafeShiftL 1 $ \i -> rep n $ \j -> when (j .&. i == 0) $ do
    item <- VUM.unsafeRead g (j .|. i)
    VUM.unsafeModify g (+ item) j
  return g

fmtAND :: VU.Vector Int -> VU.Vector Int
fmtAND vec = VU.create $ do
  let
    !f = growVU vec
    !n = VU.length f
  g <- VU.unsafeThaw f
  forG 1 (n - 1) const 0 unsafeShiftL 1 $ \i -> rep n $ \j -> when (j .&. i == 0) $ do
    item <- VUM.unsafeRead g (j .|. i)
    VUM.unsafeModify g (subtract item) j
  return g

{- ゼータ変換とメビウス変換(bitOR)
上位集合のときには部分集合{1}を含んでいる部分集合Tについての総和を求めた。
これに対して部分集合{1}が含んでいる部分集合Tについての総和を求めるものがこれ
A    = { 0, 1, 2 } <- サイズ2^nの配列におけるnがAの大きさ
S    = { { }, {0}, {1}, {2}, {0, 1}, {0, 2}, {1, 2}, {0, 1, 2} }
f(S) =    3 ,  1 ,  4 ,  1 ,   5   ,   9   ,    2  ,     6
Si   = { 1 }
T    = { { }, {1} }
f(T) =    3 +  4 = 7
これをすべての部分集合について行う

TODO: 和以外も
-}

fztOR :: VU.Vector Int -> VU.Vector Int
fztOR vec = VU.create $ do
  let
    !f = growVU vec
    !n = VU.length f
  g <- VU.unsafeThaw f
  forG 1 (n - 1) const 0 unsafeShiftL 1 $ \i -> rep n $ \j -> when (j .&. i == 0) $ do
    item <- VUM.unsafeRead g j
    VUM.unsafeModify g (+ item) (j .|. i)
  return g

fmtOR :: VU.Vector Int -> VU.Vector Int
fmtOR vec = VU.create $ do
  let
    !f = growVU vec
    !n = VU.length f
  g <- VU.unsafeThaw f
  forG 1 (n - 1) const 0 unsafeShiftL 1 $ \i -> rep n $ \j -> when (j .&. i == 0) $ do
    item <- VUM.unsafeRead g j
    VUM.unsafeModify g (subtract item) (j .|. i)
  return g

{- xorによるゼータ変換とメビウス変換
????
ブール関数とウォルシュ行列の積はXORによるメビウス変換に一致する
これを高速ウォルシュ変換という
1 ->  1 ->  3 ->  4
0 ->  1 ->  1 ->  2
1 ->  2 -> -1 ->  0
0 ->  0 ->  1 -> -2
0 ->  1 ->  1 ->  0
1 ->  1 -> -1 ->  2
1 ->  0 ->  1 ->  0
0 ->  0 -> -1 ->  2
-}
fztXOR :: VU.Vector Int -> VU.Vector Int
fztXOR vec = VU.create $ do
  let
    !f = growVU vec
    !n = VU.length f
  g <- VU.unsafeThaw f
  forG 1 (n - 1) const 0 unsafeShiftL 1 $ \i -> rep n $ \j -> when (j .&. i == 0) $ do
    itemX <- VUM.unsafeRead g j
    itemY <- VUM.unsafeRead g (j .|. i)
    VUM.unsafeWrite g j (itemX + itemY)
    VUM.unsafeWrite g (j .|. i) (itemX - itemY)
  return g

fmtXOR :: VU.Vector Int -> VU.Vector Int
fmtXOR vec = VU.create $ do
  let
    !f = growVU vec
    !n = VU.length f
  g <- VU.unsafeThaw f
  forG 1 (n - 1) const 0 unsafeShiftL 1 $ \i -> rep n $ \j -> when (j .&. i == 0) $ do
    itemX <- VUM.unsafeRead g j
    itemY <- VUM.unsafeRead g (j .|. i)
    VUM.unsafeWrite g j ((itemX + itemY) `div` 2)
    VUM.unsafeWrite g (j .|. i) ((itemX - itemY) `div` 2)
  return g


{- 約数によるゼータ変換とメビウス変換
bitANDとbitORによるゼータ変換とメビウス変換では部分集合の包含関係によって
和を求めるスコアを選んでいた
約数によるゼータ変換とメビウス変換ではある部分集合の包含を、約数かどうか
によって考え、得点の総和を求める
S  = {0, 1, 2, 3, 4, 5, 6, 7}
S  =  3  1  4  1  5  9  2  6
S4 =  4 (= 1, 2, 4)
T  =  1 + 4 + 5 = 10
S6 =  6 (= 1, 2, 3, 6)
T  =  1 + 4 + 1 + 2 = 8
Zeta = 3, 1, 5, 2, 10, 10, 8, 7
-}
fztDIV :: VU.Vector Int -> VU.Vector Int
fztDIV vec = VU.create $ do
  let !n = VU.length vec
  sieve <- VUM.replicate n True
  g <- VU.unsafeThaw vec
  for_ 2 (n - 1) 1 $ \p -> do
    b <- VUM.unsafeRead sieve p
    when b $ forG 1 (n - 1) (*) p (+) 1 $ \k -> do
      VUM.unsafeWrite sieve (k * p) False
      VUM.unsafeModify g (+(vec VU.! k)) (k * p)
  return g

fmtDIV :: VU.Vector Int -> VU.Vector Int
fmtDIV vec = VU.create $ do
  let !n = VU.length vec
  sieve <- VUM.replicate n True
  g <- VU.unsafeThaw vec
  for_ 2 (n - 1) 1 $ \p -> do
    b <- VUM.unsafeRead sieve p
    when b $ forR ((n - 1) `div` p) 1 1 $ \k -> do
      VUM.unsafeWrite sieve (k * p) False
      VUM.unsafeModify g (subtract (vec VU.! k)) (k * p)
  return g

growVU :: VU.Vector Int -> VU.Vector Int
growVU v
  | VU.null v = VU.singleton 0
  | VU.length v == 1 = v
  | otherwise = v VU.++ VU.replicate (ceilPow2 n - n) 0
  where !n = VU.length v

clz :: FiniteBits fb => fb -> Int
clz = countLeadingZeros
{-# INLINE clz #-}
(.>>>.) :: Int -> Int -> Int
(.>>>.) (I# x#) (I# i#) = I# (uncheckedIShiftRL# x# i#)
{-# INLINE (.>>>.) #-}
ceilPow2 :: Int -> Int
ceilPow2 n
  | n > 1     = (-1) .>>>. clz (n - 1) + 1
  | otherwise = 1
{-# INLINE ceilPow2 #-}

-- | l -> x -> r, +d
stream :: Monad m => Int -> Int -> Int -> VFSM.Stream m Int
stream !l !r !d = VFSM.Stream step l
  where
    step x
      | x <= r    = return $ VFSM.Yield x (x + d)
      | otherwise = return VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] stream #-}

-- | 0 <= x < n, interval = 1
rep :: Monad m => Int -> (Int -> m ()) -> m ()
rep n = flip VFSM.mapM_ (stream 0 (n - 1) 1)
{-# INLINE rep #-}

-- | 0 <= x <= n, interval = 1
rep' :: Monad m => Int -> (Int -> m ()) -> m ()
rep' n = flip VFSM.mapM_ (stream 0 n 1)
{-# INLINE rep' #-}

-- | 1 <= x < n, interval = 1
rep1 :: Monad m => Int -> (Int -> m ()) -> m ()
rep1 n = flip VFSM.mapM_ (stream 1 (n - 1) 1)
{-# INLINE rep1 #-}

-- | 1 <= x <= n, interval = 1
rep1' :: Monad m => Int -> (Int -> m ()) -> m ()
rep1' n = flip VFSM.mapM_ (stream 1 n 1)
{-# INLINE rep1' #-}

-- | l <= x <= r, interval = d
for_ :: Monad m => Int -> Int -> Int -> (Int -> m ()) -> m ()
for_ l r d = flip VFSM.mapM_ (stream l r d)
{-# INLINE for_ #-}

-- | r -> x -> l, -d
streamR :: Monad m => Int -> Int -> Int -> VFSM.Stream m Int
streamR !r !l !d = VFSM.Stream step r
  where
    step x
      | x >= l    = return $ VFSM.Yield x (x - d)
      | otherwise = return VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] streamR #-}

-- | n > x >= 0, interval = -1
rev :: Monad m => Int -> (Int -> m ()) -> m ()
rev n = flip VFSM.mapM_ (streamR (n - 1) 0 1)
{-# INLINE rev #-}

-- | n >= x >= 0, interval = -1
rev' :: Monad m => Int -> (Int -> m ()) -> m ()
rev' n = flip VFSM.mapM_ (streamR n 0 1)
{-# INLINE rev' #-}

-- | n > x >= 1, interval = -1
rev1 :: Monad m => Int -> (Int -> m ()) -> m ()
rev1 n = flip VFSM.mapM_ (streamR (n - 1) 1 1)
{-# INLINE rev1 #-}

-- | n >= x >= 1, interval = -1
rev1' :: Monad m => Int -> (Int -> m ()) -> m ()
rev1' n = flip VFSM.mapM_ (streamR n 1 1)
{-# INLINE rev1' #-}

-- | r >= x >= l, interval = -d
forR :: Monad m => Int -> Int -> Int -> (Int -> m ()) -> m ()
forR r l d = flip VFSM.mapM_ (streamR r l d)
{-# INLINE forR #-}

-- | for (int i = l; f(i, p) <= r ; g(i, d))
streamG :: Monad m => Int -> Int -> (Int -> Int -> Int) -> Int -> (Int -> Int -> Int) -> Int -> VFSM.Stream m Int
streamG l r f p g d = VFSM.Stream step l
  where
    step x
      | f x p <= r = return $ VFSM.Yield x (g x d)
      | otherwise  = return VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] streamG #-}

forG :: Monad m => Int -> Int -> (Int -> Int -> Int) -> Int -> (Int -> Int -> Int) -> Int -> (Int -> m ()) -> m ()
forG l r f p g d = flip VFSM.mapM_ (streamG l r f p g d)
{-# INLINE forG #-}

withBreakIO :: ((r -> ContT r IO b) -> ContT r IO r) -> IO r
withBreakIO = flip runContT pure . callCC
{-# INLINE withBreakIO #-}

withBreakST :: ((r -> ContT r (ST s) b) -> ContT r (ST s) r) -> (ST s) r
withBreakST = flip runContT pure . callCC
{-# INLINE withBreakST #-}
