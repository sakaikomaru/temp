{-# OPTIONS_GHC -mavx2            #-}
{-# OPTIONS_GHC -O3               #-}
{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module PollardRhoIO where

import           Control.Monad
import           Control.Monad.Cont
import           Control.Monad.ST
import           Control.Monad.State
import           Data.Bits
import           Data.Char
import           Data.Coerce
import qualified Data.Foldable                     as F
import           Data.Int
import           Data.IORef
import           Data.Maybe
import           Data.STRef
import           Data.Word
import           GHC.Exts
import           System.CPUTime
import           System.IO
import           Unsafe.Coerce
import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Builder           as BSB
import qualified Data.ByteString.Char8             as BSC8
import qualified GHC.Integer.GMP.Internals         as GMP
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM
import qualified Data.Vector.Generic               as VG
import qualified Data.Vector.Unboxed               as VU
import qualified Data.Vector.Unboxed.Mutable       as VUM

------------------------------------------------------------------------------
-- pollard rho
-------------------------------------------------------------------------------
pollardRho :: Int -> IO Int
pollardRho n = do
  xRef <- newIORef (2 :: Int)
  yRef <- newIORef (2 :: Int)
  dRef <- newIORef (1 :: Int)
  rRef <- newIORef (1 :: Int)
  withBreakIO $ \break -> fix $ \loop -> do
    d <- liftIO $ readIORef dRef
    when (d /= 1) $ do
      if d /= n
        then break ()
        else do
          liftIO $ writeIORef xRef (2 :: Int)
          liftIO $ writeIORef yRef (2 :: Int)
          liftIO $ writeIORef dRef (1 :: Int)
          liftIO $ modifyIORef rRef succ
          loop
    r <- liftIO $ readIORef rRef
    liftIO $ modifyIORef xRef (`nextF` r)
    liftIO $ modifyIORef yRef (flip nextF r . flip nextF r)
    itemX <- liftIO $ readIORef xRef
    itemY <- liftIO $ readIORef yRef
    liftIO $ writeIORef dRef (gcd (abs (itemX - itemY)) n)
    loop
  readIORef dRef
  where
    nextF :: Int -> Int -> Int
    nextF x c = fI $ (fi x * fi x + fi c) `mod` fi n

factorize :: Int -> VUM.IOVector Int -> IORef Int -> IO ()
factorize n mvec ptr
  | n == 1 = return ()
  | n == 2 = do
    _ptr <- readIORef ptr
    VUM.unsafeWrite mvec _ptr 2
    modifyIORef' ptr succ
  | even n = do
    let
      !_s = ctz n
      !_d = n .>>. _s
    _ptr <- readIORef ptr
    rep _s $ \i -> VUM.unsafeWrite mvec (_ptr + i) 2
    modifyIORef' ptr (+ _s)
    factorize _d mvec ptr
  | millerRabin n = do
    _ptr <- readIORef ptr
    VUM.unsafeWrite mvec _ptr n
    modifyIORef' ptr succ
  | otherwise = do
    m <- pollardRho n
    let k = n `div` m
    if millerRabin m
      then do
        _ptr <- readIORef ptr
        VUM.unsafeWrite mvec _ptr m
        modifyIORef' ptr succ
        factorize k mvec ptr
      else do
        _ptr <- readIORef ptr
        l <- pollardRho m
        VUM.unsafeWrite mvec _ptr l
        modifyIORef' ptr succ
        factorize (m `div` l) mvec ptr
        factorize k mvec ptr

-------------------------------------------------------------------------------
-- miller rabin
-------------------------------------------------------------------------------
millerRabin :: Int -> Bool
millerRabin k
  | k <= 3 = k == 2 || k == 3
  | k .&. 1 == 0 = False
  | otherwise = _millerRabin k
  where
    _millerRabin :: Int -> Bool
    _millerRabin n
      | n < 2047            = loop [2]
      | n < 1373653         = loop [2,3]
      | n < 9080191         = loop [31,73]
      | n < 25326001        = loop [2,3,5]
      | n < 4759123141      = loop [2,7,61]
      | n < 1122004669633   = loop [2,13,23,1662803]
      | n < 2152302898747   = loop [2,3,5,7,11]
      | n < 3474749660383   = loop [2,3,5,7,11,13]
      | n < 341550071728321 = loop [2,3,5,7,11,13,17]
      | otherwise           = loop [2,325,9375,28178,450775,9780504,1795265022]
      where
        !m = n - 1
        !s = ctz m
        !d = m .>>. s
        loop :: VU.Vector Int -> Bool
        loop vec = runST $ do
          ret <- newSTRef True
          withBreakST $ \break1 -> do
            VU.forM_ vec $ \a -> do
              let
                check1 = powModInt a d n /= 1
                check2 = check3 a
              when (check1 && check2) $ do
                lift $ writeSTRef ret False
                break1 ()
          readSTRef ret
          where
            check3 :: Int -> Bool
            check3 aa = runST $ do
              retRef <- newSTRef True
              withBreakST $ \break2 -> rep s $ \r -> when (powModInt aa ((1 .<<. r) * d) n == m) $ do
                lift $ writeSTRef retRef False
                break2 ()
              readSTRef retRef

-------------------------------------------------------------------------------
-- radix sort
-------------------------------------------------------------------------------
radixSort64 :: VU.Vector Word64 -> VU.Vector Word64
radixSort64 vword = F.foldl' step vword ([0,16,32,48] :: [Int])
  where
    mask k x = fromIntegral $ x .>>. k .&. 0xffff
    step v k = VU.create $ do
      pref <- VU.unsafeThaw
            . VU.prescanl' (+) 0
            . VU.unsafeAccumulate (+) (VU.replicate 0x10000 0)
            $ VU.map ((, 1) . mask k) v
      res <- VUM.unsafeNew $ VU.length v
      VU.forM_ v $ \x -> do
        let !masked = mask k x
        i <- VUM.unsafeRead pref masked
        VUM.unsafeWrite pref masked $ i + 1
        VUM.unsafeWrite res i x
      return res
{-# INLINE radixSort64 #-}

radixSort :: (VU.Unbox a, Word64Encode a) => VU.Vector a -> VU.Vector a
radixSort = VU.map decode64 . radixSort64 . VU.map encode64
{-# INLINE radixSort #-}

radixSortInt :: VU.Vector Int -> VU.Vector Int
radixSortInt = unsafeCoerce . radixSort64 . unsafeCoerce

radixSortNonNegative :: (VU.Unbox a, Word64Encode a) => VU.Vector a -> VU.Vector a
radixSortNonNegative = VU.map decodeNonNegative64 . radixSort64 . VU.map encodeNonNegative64
{-# INLINE radixSortNonNegative #-}

radixSort32 :: VU.Vector Word32 -> VU.Vector Word32
radixSort32 vec = F.foldl' step vec ([0, 16] :: [Int])
  where
    mask k x = fromIntegral $ x .>>. k .&. 0xffff
    step v k = VU.create $ do
      pref <- VU.unsafeThaw
            . VU.prescanl' (+) 0
            . VU.unsafeAccumulate (+) (VU.replicate 0x10000 0)
            $ VU.map ((, 1) . mask k) v
      res <- VUM.unsafeNew $ VU.length v
      VU.forM_ v $ \x -> do
        let !masked = mask k x
        i <- VUM.unsafeRead pref masked
        VUM.unsafeWrite pref masked $ i + 1
        VUM.unsafeWrite res i x
      return res
{-# INLINE radixSort32 #-}

compress :: VU.Vector Int -> VU.Vector Int
compress vec = VU.create $ do
  mvec <- VUM.unsafeNew (VU.length vec)
  VU.mapM_ (\(i, x) -> VUM.unsafeWrite mvec (x .&. 0xffffffff) i) . VU.postscanl' (\(!i, !x) y ->
        if x .>>. 32 == y .>>. 32
          then (i, y)
          else (i + 1, y)
      ) (-1, -1)
    . radixSortInt
    $ VU.imap (\i x -> x .<<. 32 .|. i) vec
  return mvec
{-# INLINE compress #-}

class Word64Encode a where
  encode64 :: a -> Word64
  decode64 :: Word64 -> a
  encodeNonNegative64 :: a -> Word64
  encodeNonNegative64 = encode64
  decodeNonNegative64 :: Word64 -> a
  decodeNonNegative64 = decode64

instance Word64Encode Int where
  encode64 x = unsafeCoerce $ x + 0x3fffffffffffffff
  decode64 x = unsafeCoerce x - 0x3fffffffffffffff
  encodeNonNegative64 = unsafeCoerce
  decodeNonNegative64 = unsafeCoerce

instance Word64Encode (Int, Int) where
  encode64 (x, y) = unsafeCoerce
    $ (x + 0x3fffffff) .<<. 31 .|. (y + 0x3fffffff)
  decode64 xy = unsafeCoerce (x, y)
    where
      !x = xy .>>. 31 - 0x3fffffff
      !y = (xy .&. 0x7fffffff) - 0x3fffffff
  encodeNonNegative64 (x, y) = unsafeCoerce $ x .<<. 31 .|. y
  decodeNonNegative64 xy     = unsafeCoerce (x, y)
    where
      !x = xy .>>. 31
      !y = xy .&. 0x7fffffff

instance Word64Encode (Int, Int, Int) where
  encode64 (x, y, z) = unsafeCoerce $ ((x + 0xfffff) .<<. 21 .|. (y + 0xfffff)) .<<. 21 .|. (z + 0xfffff)
  decode64 xyz = unsafeCoerce (x, y, z)
    where
      !x = xyz .>>. 42 - 0xfffff
      !y = (xyz .>>. 21 .&. 0x1fffff) - 0xfffff
      !z = xyz .&. 0x1fffff - 0xfffff
  encodeNonNegative64 (x, y, z) = unsafeCoerce $ (x .<<. 21 .|. y) .<<. 21 .|. z
  decodeNonNegative64 xyz = unsafeCoerce (x, y, z)
    where
      !x = xyz .>>. 42
      !y = xyz .>>. 21 .&. 0x1fffff
      !z = xyz .&. 0x1fffff

-------------------------------------------------------------------------------
-- permuted congruential generator
-------------------------------------------------------------------------------
seed :: Word64
seed = 0x4d595df4d0f33173
{-# INLINE seed #-}

multiplier :: Word64
multiplier = 0x5851f42d4c957f2d
{-# INLINE multiplier #-}

increment :: Word64
increment = 0x14057b7ef767814f
{-# INLINE increment #-}

type RNG = VUM.IOVector Word64

newRNG :: IO RNG
newRNG = do
  t <- fromInteger <$> getCPUTime
  VUM.replicate 1 (seed + t)

nextWord32 :: RNG -> IO Word32
nextWord32 rng = do
  x1 <- VUM.unsafeRead rng 0
  let
    cnt   = x1 .>>. 59
    state = x1 * multiplier + increment
    x2    = x1 .^. (x1 .>>. 18)
  VUM.unsafeWrite rng 0 state
  return $ unsafeCoerce (x2 .>>. 27) .>>@. unsafeCoerce @Word64 @Int cnt

nextWord :: RNG -> IO Word
nextWord rng = do
  w1 <- unsafeCoerce @Word32 @Word <$> nextWord32 rng
  w2 <- unsafeCoerce @Word32 @Word <$> nextWord32 rng
  return $ w1 .<<.32 .|. w2

nextInt :: RNG -> IO Int
nextInt = unsafeCoerce <$> nextWord

nextDouble :: RNG -> IO Double
nextDouble rng = do
  t <- nextWord rng
  let x = 0x3ff .<<. 52 .|. t .>>. 12
  return $! unsafeCoerce @Word @Double x - 1.0

nextGauss :: RNG -> Double -> Double -> IO Double
nextGauss rng mu sigma = do
  x <- nextDouble rng
  y <- nextDouble rng
  let z = sqrt (-2.0 * log x) * cos (2.0 * pi * y)
  return $! sigma * z + mu

randomR :: RNG -> Int -> Int -> IO Int
randomR rng l r = (+ l) . flip mod (r - l + 1) <$> nextInt rng

shuffleM :: VUM.Unbox a => RNG -> VUM.IOVector a -> IO ()
shuffleM rng mvec = do
  rev (VUM.length mvec) $ \i -> do
    j <- nextWord rng
    VUM.unsafeSwap mvec i (unsafeCoerce $ rem j (unsafeCoerce i + 1))

shuffle :: VU.Unbox a => RNG -> VU.Vector a -> IO (VU.Vector a)
shuffle rng vec = do
  mv <- VU.unsafeThaw vec
  shuffleM rng mv
  VU.unsafeFreeze mv

infixr 8 .>>@.

(.>>@.) :: Word32 -> Int -> Word32
x .>>@. k = x .>>. k .|. x .<<. ((-k) .&. 31)
{-# INLINE (.>>@.) #-}

-------------------------------------------------------------------------------
-- util
-------------------------------------------------------------------------------
fi :: Int -> Integer
fi = fromIntegral
{-# INLINE fi #-}

fI :: Integer -> Int
fI = fromInteger
{-# INLINE fI #-}

powModInt :: Int -> Int -> Int -> Int
powModInt a b c = fI $ GMP.powModInteger (fi a) (fi b) (fi c)
{-# INLINE powModInt #-}

recipModInt :: Int -> Int -> Int
recipModInt a m = fI $ GMP.recipModInteger (fi a) (fi m)
{-# INLINE recipModInt #-}

infixl 8 .<<., .>>., .>>>.
infixl 6 .^.

(.<<.) :: Bits b => b -> Int -> b
(.<<.) = unsafeShiftL
{-# INLINE (.<<.) #-}

(.>>.) :: Bits b => b -> Int -> b
(.>>.) = unsafeShiftR
{-# INLINE (.>>.) #-}

(.>>>.) :: Int -> Int -> Int
(.>>>.) (I# x#) (I# i#) = I# (uncheckedIShiftRL# x# i#)
{-# INLINE (.>>>.) #-}

(.^.) :: Bits b => b -> b -> b
(.^.)  = xor
{-# INLINE (.^.)  #-}

clz :: FiniteBits fb => fb -> Int
clz = countLeadingZeros
{-# INLINE clz #-}

ctz :: FiniteBits fb => fb -> Int
ctz = countTrailingZeros
{-# INLINE ctz #-}

encode32x2 :: Int -> Int -> Int
encode32x2 x y = x .<<. 32 .|. y
{-# INLINE encode32x2 #-}

decode32x2 :: Int -> (Int, Int)
decode32x2 xy =
    let !x = xy .>>>. 32
        !y = xy .&. 0xffffffff
    in (x, y)
{-# INLINE decode32x2 #-}

ceilPow2 :: Int -> Int
ceilPow2 n
  | n > 1     = (-1) .>>>. clz (n - 1) + 1
  | otherwise = 1
{-# INLINE ceilPow2 #-}

floorPow2 :: Int -> Int
floorPow2 n
  | n >= 1    = 1 .<<. (63 - clz n)
  | otherwise = 0
{-# INLINE floorPow2 #-}

bitReverse :: Int -> Int
bitReverse
  = unsafeCoerce
  . step 32 0xffffffff00000000 0x00000000ffffffff
  . step 16 0xffff0000ffff0000 0x0000ffff0000ffff
  . step 08 0xff00ff00ff00ff00 0x00ff00ff00ff00ff
  . step 04 0xf0f0f0f0f0f0f0f0 0x0f0f0f0f0f0f0f0f
  . step 02 0xcccccccccccccccc 0x3333333333333333
  . step 01 0xaaaaaaaaaaaaaaaa 0x5555555555555555
  . unsafeCoerce
  where
    step :: Int -> Word64 -> Word64 -> Word64 -> Word64
    step i ml mr = \ !x -> (x .&. ml) .>>. i .|. (x .&. mr) .<<. i
    {-# INLINE step #-}

-------------------------------------------------------------------------------
-- for
-------------------------------------------------------------------------------
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

-------------------------------------------------------------------------------
-- input output
-------------------------------------------------------------------------------
type CParser a = StateT BSC8.ByteString Maybe a
runCParser :: CParser a -> BSC8.ByteString -> Maybe (a, BSC8.ByteString)
runCParser = runStateT
{-# INLINE runCParser #-}
int :: CParser Int
int = coerce $ BSC8.readInt . BSC8.dropWhile isSpace
{-# INLINE int #-}
int1 :: CParser Int
int1 = fmap (subtract 1) int
{-# INLINE int1 #-}
char :: CParser Char
char = coerce BSC8.uncons
{-# INLINE char #-}
byte :: CParser Word8
byte = coerce BS.uncons
{-# INLINE byte #-}
skipSpaces :: CParser ()
skipSpaces = modify' (BSC8.dropWhile isSpace)
{-# INLINE skipSpaces #-}
seqInput :: Int -> IO (VU.Vector Int)
seqInput n = VU.unfoldrN n (runCParser int) <$> BSC8.getLine
{-# INLINE seqInput #-}
parseN1 :: Int -> IO (VU.Vector Int)
parseN1 n = VU.unfoldrN n (runCParser int) <$> BSC8.getContents
{-# INLINE parseN1 #-}
parseN2 :: Int -> IO (VU.Vector (Int, Int))
parseN2 n = VU.unfoldrN n (runCParser $ (,) <$> int <*> int) <$> BSC8.getContents
{-# INLINE parseN2 #-}
parseN3 :: Int -> IO (VU.Vector (Int, Int, Int))
parseN3 n = VU.unfoldrN n (runCParser $ (,,) <$> int <*> int <*> int) <$> BSC8.getContents
{-# INLINE parseN3 #-}
parseN4 :: Int -> IO (VU.Vector (Int, Int, Int, Int))
parseN4 n = VU.unfoldrN n (runCParser $ (,,,) <$> int <*> int <*> int <*> int) <$> BSC8.getContents
{-# INLINE parseN4 #-}
parseN5 :: Int -> IO (VU.Vector (Int, Int, Int, Int, Int))
parseN5 n = VU.unfoldrN n (runCParser $ (,,,,) <$> int <*> int <*> int <*> int <*> int) <$> BSC8.getContents
{-# INLINE parseN5 #-}
parseANBN :: Int -> IO (VU.Vector Int, VU.Vector Int)
parseANBN n = VU.unzip . VU.unfoldrN n (runCParser $ (,) <$> int <*> int) <$> BSC8.getContents
{-# INLINE parseANBN #-}
parseANBNCN :: Int -> IO (VU.Vector Int, VU.Vector Int, VU.Vector Int)
parseANBNCN n = VU.unzip3 . VU.unfoldrN n (runCParser $ (,,) <$> int <*> int <*> int) <$> BSC8.getContents
{-# INLINE parseANBNCN #-}

type Query3 = (Int, Int, Int)
query3Parser :: CParser Query3
query3Parser = do
  skipSpaces
  t <- char
  case t of
    '0' -> (,,) 0 <$> int <*> int
    _   -> (,,) 1 <$> int <*> pure 0
parseQ3 :: Int -> IO (VU.Vector Query3)
parseQ3 n = VU.unfoldrN n (runCParser query3Parser) <$> BSC8.getContents
{-# INLINE parseQ3 #-}

type Query5 = (Int, Int, Int, Int, Int)
query5Parser :: CParser Query5
query5Parser = do
  skipSpaces
  t <- char
  case t of
    '0' -> (,,,,) 0 <$> int <*> int <*> int    <*> int
    _   -> (,,,,) 1 <$> int <*> int <*> pure 0 <*> pure 0
parseQ5 :: Int -> IO (VU.Vector Query5)
parseQ5 n = VU.unfoldrN n (runCParser query5Parser) <$> BSC8.getContents
{-# INLINE parseQ5 #-}

readInt :: BSC8.ByteString -> Int
readInt = fst . fromJust . BSC8.readInt
{-# INLINE readInt #-}
getInt :: IO Int
getInt = readInt <$> BSC8.getLine
{-# INLINE getInt #-}
readIntList :: BSC8.ByteString -> [Int]
readIntList = map readInt . BSC8.words
{-# INLINE readIntList #-}
getIntList :: IO [Int]
getIntList = readIntList <$> BSC8.getLine
{-# INLINE getIntList #-}
readInteger :: BSC8.ByteString -> Integer
readInteger = fst . fromJust . BSC8.readInteger
{-# INLINE readInteger #-}
getInteger :: IO Integer
getInteger = readInteger <$> BSC8.getLine
{-# INLINE getInteger #-}
readIntegerList :: BSC8.ByteString -> [Integer]
readIntegerList = map readInteger . BSC8.words
{-# INLINE readIntegerList #-}
getIntegerList :: IO [Integer]
getIntegerList = readIntegerList <$> BSC8.getLine
{-# INLINE getIntegerList #-}

class ShowAsBuilder a where
  showAsBuilder :: a -> BSB.Builder
  default showAsBuilder :: (Show a) => a -> BSB.Builder
  showAsBuilder = BSB.string8 . show

instance ShowAsBuilder Int where
  showAsBuilder = BSB.intDec
  {-# INLINE showAsBuilder #-}
instance ShowAsBuilder Int8 where
  showAsBuilder = BSB.int8Dec
  {-# INLINE showAsBuilder #-}
instance ShowAsBuilder Int16 where
  showAsBuilder = BSB.int16Dec
  {-# INLINE showAsBuilder #-}
instance ShowAsBuilder Int32 where
  showAsBuilder = BSB.int32Dec
  {-# INLINE showAsBuilder #-}
instance ShowAsBuilder Int64 where
  showAsBuilder = BSB.int64Dec
  {-# INLINE showAsBuilder #-}
instance ShowAsBuilder Word8 where
  showAsBuilder = BSB.word8Dec
  {-# INLINE showAsBuilder #-}
instance ShowAsBuilder Word16 where
  showAsBuilder = BSB.word16Dec
  {-# INLINE showAsBuilder #-}
instance ShowAsBuilder Word32 where
  showAsBuilder = BSB.word32Dec
  {-# INLINE showAsBuilder #-}
instance ShowAsBuilder Word64 where
  showAsBuilder = BSB.word64Dec
  {-# INLINE showAsBuilder #-}
instance ShowAsBuilder Integer where
  showAsBuilder = BSB.integerDec
  {-# INLINE showAsBuilder #-}
instance ShowAsBuilder Float where
  showAsBuilder = BSB.floatDec
  {-# INLINE showAsBuilder #-}
instance ShowAsBuilder Double where
  showAsBuilder = BSB.doubleDec
  {-# INLINE showAsBuilder #-}

instance (ShowAsBuilder a, VG.Vector v a) => ShowAsBuilder (v a) where
  showAsBuilder = v2BSpcSep

putBuilder :: BSB.Builder -> IO ()
putBuilder = BSB.hPutBuilder stdout
{-# INLINE putBuilder #-}

printVecInLines :: (VG.Vector v a, ShowAsBuilder a) => v a -> IO ()
printVecInLines = putBuilder . v2BLines
{-# INLINE printVecInLines #-}

printVecInSpcSepLn :: (VG.Vector v a, ShowAsBuilder a) => v a -> IO ()
printVecInSpcSepLn = putBuilder . v2BSpcSepLn
{-# INLINE printVecInSpcSepLn #-}

v2BSpcSepLn :: (VG.Vector v a, ShowAsBuilder a) => v a -> BSB.Builder
v2BSpcSepLn = v2BSpcSepLnWith showAsBuilder
{-# INLINE v2BSpcSepLn #-}

v2BSpcSep :: (VG.Vector v a, ShowAsBuilder a) => v a -> BSB.Builder
v2BSpcSep = v2BSpcSepWith showAsBuilder
{-# INLINE v2BSpcSep #-}

v2BConcat:: (VG.Vector v a, ShowAsBuilder a) => v a -> BSB.Builder
v2BConcat = v2BConcatWith showAsBuilder
{-# INLINE v2BConcat #-}

v2BLines:: (VG.Vector v a, ShowAsBuilder a) => v a -> BSB.Builder
v2BLines = v2BLinesWith showAsBuilder
{-# INLINE v2BLines #-}

v2BSpcSepLnWith :: VG.Vector v a => (a -> BSB.Builder) -> v a -> BSB.Builder
v2BSpcSepLnWith = v2BSpcSepPostfWith "\n"
{-# INLINE v2BSpcSepLnWith #-}

v2BSpcSepWith :: VG.Vector v a => (a -> BSB.Builder) -> v a -> BSB.Builder
v2BSpcSepWith = v2BSpcSepPostfWith ""
{-# INLINE v2BSpcSepWith #-}

v2BConcatWith :: VG.Vector v a => (a -> BSB.Builder) -> v a -> BSB.Builder
v2BConcatWith showFct = VG.foldr ((<>) . showFct) mempty
{-# INLINE v2BConcatWith #-}

v2BLinesWith :: VG.Vector v a => (a -> BSB.Builder) -> v a -> BSB.Builder
v2BLinesWith showFct = VG.foldr (\a -> (showFct a <>) . (BSB.char7 '\n' <>)) mempty
{-# INLINE v2BLinesWith #-}

v2BSpcSepPostf :: (VG.Vector v a, ShowAsBuilder a) => BS.ByteString -> v a -> BSB.Builder
v2BSpcSepPostf = (`v2BSpcSepPostfWith` showAsBuilder)
{-# INLINE v2BSpcSepPostf #-}

v2BSpcSepPostfWith :: VG.Vector v a => BS.ByteString -> (a -> BSB.Builder) -> v a -> BSB.Builder
v2BSpcSepPostfWith = vecToBuilder "" " "
{-# INLINE v2BSpcSepPostfWith #-}

vecToBuilder :: VG.Vector v a => BS.ByteString -> BS.ByteString -> BS.ByteString -> (a -> BSB.Builder) -> v a -> BSB.Builder
vecToBuilder !prefix !separator !postfix = vecToBuilder_ (BSB.byteString prefix) (BSB.byteString separator) (BSB.byteString postfix)
{-# INLINE vecToBuilder #-}

vecToBuilder_ :: VG.Vector v a => BSB.Builder -> BSB.Builder -> BSB.Builder -> (a -> BSB.Builder) -> v a -> BSB.Builder
vecToBuilder_ !prefix !separator !postfix showFct vec = prefix <> VG.foldr (\a rest !prefx -> prefx <> (showFct a <> rest separator)) (const postfix) vec mempty
{-# INLINE vecToBuilder_ #-}
