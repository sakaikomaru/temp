module SkewHeap where

data SkewHeap a
  = Null
  | Node a (SkewHeap a) (SkewHeap a)
  deriving (Eq, Show)

instance Functor SkewHeap where
  fmap _ Null = Null
  fmap f (Node x Null Null) = Node (f x) Null Null
  fmap f (Node x l r)       = Node (f x) (fmap f l) (fmap f r)

empty :: SkewHeap a
empty = Null

root :: SkewHeap a -> Maybe a
root Null = Nothing
root (Node a _ _) = Just a

merge :: Ord v => SkewHeap v -> SkewHeap v -> SkewHeap v
merge Null rhs = rhs
merge lhs Null = lhs
merge a@(Node a_val a_left a_right) b@(Node b_val b_left b_right)
    | a_val <= b_val = Node a_val (merge a_left b) a_right
    | otherwise      = Node b_val (merge b_left a) b_right

maxDepth :: SkewHeap a -> Int
maxDepth Null = 0
maxDepth (Node _ l r) = 1 + max (maxDepth l) (maxDepth r)

minDepth :: SkewHeap a -> Int
minDepth Null = 0
minDepth (Node _ l r) = 1 + min (minDepth l) (minDepth r)

getMin :: Bounded a => SkewHeap a -> a
getMin (Node x _ _) = x
getMin Null         = maxBound

insert :: Ord v => v -> SkewHeap v -> SkewHeap v
insert val = merge (Node val Null Null)

removeMin :: Ord a => SkewHeap a -> Maybe (a, SkewHeap a)
removeMin Null = Nothing
removeMin (Node min left right) = Just (min, merge left right)

remove :: Ord a => a -> SkewHeap a -> SkewHeap a
remove _ Null = Null
remove v (Node root left right)
    | v == root = merge left right
    | otherwise = Node root (remove v left) (remove v right)

update :: Ord a => a -> a -> SkewHeap a -> SkewHeap a
update old new heap = insert new (remove old heap)

commaSeparatedStr :: (Show a) => SkewHeap a -> String
commaSeparatedStr Null                  = ""
commaSeparatedStr (Node val left right) = show val ++ ", " ++ commaSeparatedStr left ++ commaSeparatedStr right