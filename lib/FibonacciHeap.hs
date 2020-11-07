module FibonacciHeap where

data BiTree a = Node
  { rank     :: !Int
  , root     :: !a
  , children :: [BiTree a]
  } deriving (Eq, Show)

data FibHeap a
  = Empty
  | FibHeap
  { size    :: Int
  , minTree :: BiTree a
  , trees   :: [BiTree a]
  } deriving (Eq, Show)

singletonFH :: a -> FibHeap a
singletonFH x = FibHeap 1 (Node 1 x []) []

isEmptyFH :: FibHeap a -> Bool
isEmptyFH Empty = True
isEmptyFH _     = False

linkFH :: Ord a => BiTree a -> BiTree a -> BiTree a
linkFH t1@(Node r x c1) t2@(Node _ y c2)
  | x < y     = Node (r + 1) x (t2 : c1)
  | otherwise = Node (r + 1) y (t1 : c2)

insertFH :: Ord a => FibHeap a -> a -> FibHeap a
insertFH h x = mergeFH h (singletonFH x)

mergeFH :: Ord a => FibHeap a -> FibHeap a -> FibHeap a
mergeFH h Empty = h
mergeFH Empty h = h
mergeFH (FibHeap sz1 minTr1 ts1) (FibHeap sz2 minTr2 ts2)
  | root minTr1 < root minTr2 = FibHeap (sz1 + sz2) minTr1 (minTr2 : ts2 ++ ts1)
  | otherwise                 = FibHeap (sz1 + sz2) minTr2 (minTr1 : ts1 ++ ts2)

findMinFH :: Ord a => FibHeap a -> a
findMinFH = root . minTree

consolidateFH :: Ord a => [BiTree a] -> [BiTree a]
consolidateFH = foldl meld [] 
  where
    meld [] t = [t]
    meld (t':ts) t 
      | rank t == rank t' = meld ts (linkFH t t')
      | rank t <  rank t' = t : t' : ts
      | otherwise         = t' : meld ts t

extractMinFH :: Ord a => [BiTree a] -> (BiTree a, [BiTree a])
extractMinFH [t]    = (t, [])
extractMinFH (t:ts) =
  if root t < root t' 
    then (t,ts)
    else (t', t:ts')
  where 
    (t', ts') = extractMinFH ts

deleteMinFH :: Ord a => FibHeap a -> FibHeap a
deleteMinFH (FibHeap _ (Node _ _ []) []) = Empty
deleteMinFH (FibHeap sz minTr ts)        = FibHeap (sz - 1) minTr' ts' 
  where
    (minTr', ts') = extractMinFH $ consolidateFH (children minTr ++ ts)

fromListFH :: Ord a => [a] -> FibHeap a
fromListFH = foldl insertFH Empty

heapSort :: Ord a => [a] -> [a]
heapSort = hsort . fromListFH 
  where
    hsort Empty = []
    hsort h     = (findMinFH h) : (hsort $ deleteMinFH h)
