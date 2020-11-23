{-# OPTIONS_GHC -mavx2                  #-}
{-# OPTIONS_GHC -O3                     #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE UnboxedTuples              #-}

module SquareMatrix where

import           Control.Monad.Cont
import           Control.Monad.ST
import           Data.Bits
import           Data.Bool
import           Data.Coerce
import qualified Data.Ratio                        as R
import           GHC.Exts
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM
import qualified Data.Vector.Generic               as VG
import qualified Data.Vector.Generic.Mutable       as VGM
import qualified Data.Vector.Unboxed               as VU
import qualified Data.Vector.Unboxed.Mutable       as VUM

#define MOD 998244353

type SquareMatrixMint = VU.Vector Mint

infixr 8 <|^|>
infixr 7 <|#|>
infixl 7 <|*|>
infixl 6 <|+|>, <|-|>

matO :: Int -> SquareMatrixMint
matO sz = VU.replicate sz (0 :: Mint) 
{-# INLINE matO #-}

matE :: Int -> SquareMatrixMint
matE sz = VU.imap (\i _ -> bool 0 1 (i `mod` (sz + 1) == 0)) $ VU.replicate (sz * sz) (0 :: Mint)
{-# INLINE matE #-}

buildMatrix :: VU.Vector Int -> SquareMatrixMint
buildMatrix = VU.map mint
{-# INLINE buildMatrix #-}

(<|+|>) :: SquareMatrixMint -> SquareMatrixMint -> SquareMatrixMint
a <|+|> b = VU.zipWith (+) a b
{-# INLINE (<|+|>) #-}

(<|-|>) :: SquareMatrixMint -> SquareMatrixMint -> SquareMatrixMint
a <|-|> b = VU.zipWith (-) a b
{-# INLINE (<|-|>) #-}

(<|*|>) :: SquareMatrixMint -> SquareMatrixMint -> SquareMatrixMint
a <|*|> b = VU.create $ do
  c <- VUM.unsafeNew m :: ST s (VUM.STVector s Mint)
  rep sz $ \i -> rep sz $ \j -> rep sz $ \k -> VUM.unsafeModify c (+ (a VU.! (i * sz + k)) * (b VU.! (k * sz + j))) (i * sz + j)
  return c
  where
    !m  = VU.length a
    !sz = floor . sqrt . fromIntegral $ m

(<|^|>) :: SquareMatrixMint -> Int -> SquareMatrixMint
a <|^|> n
  | n == 1    = a
  | n == 0    = matE sz
  | even n    = z <|*|> z
  | otherwise = a <|*|> (z <|*|> z)
  where
    z   = a <|^|> (n `div` 2)
    !m  = VU.length a
    !sz = floor . sqrt . fromIntegral $ m

(<|#|>) :: Int -> SquareMatrixMint -> SquareMatrixMint
n <|#|> a = VU.map (* mint n) a
{-# INLINE (<|#|>) #-}

transposeMat :: SquareMatrixMint -> SquareMatrixMint
transposeMat a = VU.create $ do
  let
    !n  = VU.length a
    !sz = floor . sqrt . fromIntegral $ n
  b <- VUM.unsafeNew n :: ST s (VUM.STVector s Mint)
  rep sz $ \i -> rep sz $ \j -> do
    VUM.unsafeWrite b (j * sz + i) (a VU.! (i * sz + j))
  return b

modulus :: Num a => a
modulus = MOD
{-# INLINE modulus #-}

infixr 8 ^%
infixl 7 *%, /%
infixl 6 +%, -%

(+%) :: Int -> Int -> Int
(I# x#) +% (I# y#) = case x# +# y# of
  r# -> I# (r# -# ((r# >=# MOD#) *# MOD#))
{-# INLINE (+%) #-}
(-%) :: Int -> Int -> Int
(I# x#) -% (I# y#) = case x# -# y# of
  r# -> I# (r# +# ((r# <# 0#) *# MOD#))
{-# INLINE (-%) #-}
(*%) :: Int -> Int -> Int
(I# x#) *% (I# y#) = case timesWord# (int2Word# x#) (int2Word# y#) of
  z# -> case timesWord2# z# im# of
    (# q#, _ #) -> case minusWord# z# (timesWord# q# m#) of
      v#  | isTrue# (geWord# v# m#) -> I# (word2Int# (plusWord# v# m#))
          | otherwise -> I# (word2Int# v#)
  where
    m#  = int2Word# MOD#
    im# = plusWord# (quotWord# 0xffffffffffffffff## m#) 1##
{-# INLINE (*%) #-}
(/%) :: Int -> Int -> Int
(I# x#) /% (I# y#) = go# y# MOD# 1# 0#
  where
    go# a# b# u# v#
      | isTrue# (b# ># 0#) = case a# `quotInt#` b# of
        q# -> go# b# (a# -# (q# *# b#)) v# (u# -# (q# *# v#))
      | otherwise = I# ((x# *# (u# +# MOD#)) `remInt#` MOD#)
{-# INLINE (/%) #-}
(^%) :: Int -> Int -> Int
x ^% n
  | n > 0  = go 1 x n
  | n == 0 = 1
  | otherwise = go 1 (1 /% x) (-n)
  where
    go !acc !y !m
      | m .&. 1 == 0 = go acc (y *% y) (unsafeShiftR m 1)
      | m == 1       = acc *% y
      | otherwise    = go (acc *% y) (y *% y) (unsafeShiftR (m - 1) 1)

newtype Mint = Mint { getMint :: Int }
  deriving newtype (Eq, Ord, Read, Show, Real)

mint :: Integral a => a -> Mint
mint x = fromIntegral $ mod (fromIntegral x) MOD
{-# INLINE mint #-}

mintValidate :: Mint -> Bool
mintValidate (Mint x) = 0 <= x && x < MOD
{-# INLINE mintValidate #-}

instance Bounded Mint where
  minBound = Mint 0
  maxBound = Mint $ modulus - 1

instance Enum Mint where
  toEnum = mint
  fromEnum = coerce

instance Integral Mint where
  quotRem x y = (x / y, x - x / y * y)
  toInteger = coerce (toInteger @Int)

instance Num Mint where
  (+) = coerce (+%)
  (-) = coerce (-%)
  (*) = coerce (*%)
  abs = id
  signum = const (Mint 1)
  fromInteger x = coerce @Int @Mint . fromInteger $ mod x modulus

instance Fractional Mint where
  (/) = coerce (/%)
  fromRational q = fromInteger (R.numerator q) / fromInteger (R.denominator q)

newtype instance VUM.MVector s Mint = MV_Mint (VUM.MVector s Int)
newtype instance VU.Vector Mint = V_Mint (VU.Vector Int)

instance VU.Unbox Mint

instance VGM.MVector VUM.MVector Mint where
  basicLength (MV_Mint v) = VGM.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (MV_Mint v) = MV_Mint $ VGM.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_Mint v1) (MV_Mint v2) = VGM.basicOverlaps v1 v2
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_Mint `fmap` VGM.basicUnsafeNew n
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_Mint v) = VGM.basicInitialize v
  {-# INLINE basicInitialize #-}
  basicUnsafeReplicate n x = MV_Mint `fmap` VGM.basicUnsafeReplicate n (coerce x)
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeRead (MV_Mint v) i = coerce `fmap` VGM.basicUnsafeRead v i
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_Mint v) i x = VGM.basicUnsafeWrite v i (coerce x)
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV_Mint v) = VGM.basicClear v
  {-# INLINE basicClear #-}
  basicSet (MV_Mint v) x = VGM.basicSet v (coerce x)
  {-# INLINE basicSet #-}
  basicUnsafeCopy (MV_Mint v1) (MV_Mint v2) = VGM.basicUnsafeCopy v1 v2
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV_Mint v1) (MV_Mint v2) = VGM.basicUnsafeMove v1 v2
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeGrow (MV_Mint v) n = MV_Mint `fmap` VGM.basicUnsafeGrow v n
  {-# INLINE basicUnsafeGrow #-}

instance VG.Vector VU.Vector Mint where
  basicUnsafeFreeze (MV_Mint v) = V_Mint `fmap` VG.basicUnsafeFreeze v
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_Mint v) = MV_Mint `fmap` VG.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_Mint v) = VG.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (V_Mint v) = V_Mint $ VG.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_Mint v) i = coerce `fmap` VG.basicUnsafeIndexM v i
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_Mint mv) (V_Mint v) = VG.basicUnsafeCopy mv v
  elemseq _ = seq
  {-# INLINE elemseq #-}

{-
rep    => [0 .. (n - 1)]
rep'   => [0 .. n]
rep1   => [1 .. (n - 1)]
rep1'  => [1 .. n]
rev    => [(n - 1) .. 0]
rev'   => [n .. 0]
rev1   => [(n - 1) .. 1]
rev1'  => [n .. 1]
range  => for (int i = l; i <= r; i++);
rangeR => for (int i = r; i >= l; i--);
forP   => for (int i = 2; i * i <= p; i++);
forG   => for (int i = l; f(i, p) <= r; g(i,d));
forRG  => for (int i = r; f(i, p) >= l; g(i,d));
-}

rep :: Monad m => Int -> (Int -> m ()) -> m ()
rep n = flip VFSM.mapM_ (streamG 0 (n - 1) const 0 (+) 1)
{-# INLINE rep #-}

rep' :: Monad m => Int -> (Int -> m ()) -> m ()
rep' n = flip VFSM.mapM_ (streamG 0 n const 0 (+) 1)
{-# INLINE rep' #-}

rep1 :: Monad m => Int -> (Int -> m ()) -> m ()
rep1 n = flip VFSM.mapM_ (streamG 1 (n - 1) const 0 (+) 1)
{-# INLINE rep1 #-}

rep1' :: Monad m => Int -> (Int -> m ()) -> m ()
rep1' n = flip VFSM.mapM_ (streamG 1 n const 0 (+) 1)
{-# INLINE rep1' #-}

rev :: Monad m => Int -> (Int -> m ()) -> m ()
rev n = flip VFSM.mapM_ (streamRG (n - 1) 0 const 0 (-) 1)
{-# INLINE rev #-}

rev' :: Monad m => Int -> (Int -> m ()) -> m ()
rev' n = flip VFSM.mapM_ (streamRG n 0 const 0 (-) 1)
{-# INLINE rev' #-}

rev1 :: Monad m => Int -> (Int -> m ()) -> m ()
rev1 n = flip VFSM.mapM_ (streamRG (n - 1) 1 const 0 (-) 1)
{-# INLINE rev1 #-}

rev1' :: Monad m => Int -> (Int -> m ()) -> m ()
rev1' n = flip VFSM.mapM_ (streamRG n 1 const 0 (-) 1)
{-# INLINE rev1' #-}

range :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
range l r = flip VFSM.mapM_ (streamG l r const 0 (+) 1)
{-# INLINE range #-}

rangeR :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
rangeR r l = flip VFSM.mapM_ (streamRG r l const 0 (-) 1)
{-# INLINE rangeR #-}

forP :: Monad m => Int -> (Int -> m ()) -> m ()
forP p = flip VFSM.mapM_ (streamG 2 p (^) 2 (+) 1)
{-# INLINE forP #-}

forG :: Monad m => Int -> Int -> (Int -> Int -> Int) -> Int -> (Int -> Int -> Int) -> Int -> (Int -> m ()) -> m ()
forG l r f p g d = flip VFSM.mapM_ (streamG l r f p g d)
{-# INLINE forG #-}

forRG :: Monad m => Int -> Int -> (Int -> Int -> Int) -> Int -> (Int -> Int -> Int) -> Int -> (Int -> m ()) -> m ()
forRG r l f p g d = flip VFSM.mapM_ (streamRG r l f p g d)
{-# INLINE forRG #-}

streamG :: Monad m => Int -> Int -> (Int -> Int -> Int) -> Int -> (Int -> Int -> Int) -> Int -> VFSM.Stream m Int
streamG !l !r !f !p !g !d = VFSM.Stream step l
  where
    step x
      | f x p <= r = return $ VFSM.Yield x (g x d)
      | otherwise  = return VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] streamG #-}

streamRG :: Monad m => Int -> Int -> (Int -> Int -> Int) -> Int -> (Int -> Int -> Int) -> Int -> VFSM.Stream m Int
streamRG !r !l !f !p !g !d = VFSM.Stream step r
  where
    step x
      | f x p >= l = return $ VFSM.Yield x (g x d)
      | otherwise  = return VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] streamRG #-}

withBreakIO :: ((r -> ContT r IO b) -> ContT r IO r) -> IO r
withBreakIO = flip runContT pure . callCC
{-# INLINE withBreakIO #-}

withBreakST :: ((r -> ContT r (ST s) b) -> ContT r (ST s) r) -> (ST s) r
withBreakST = flip runContT pure . callCC
{-# INLINE withBreakST #-}
