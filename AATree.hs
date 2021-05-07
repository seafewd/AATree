{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------

module AATree (
  AATree,        -- type of AA search trees
  emptyTree,     -- AATree a
  get,           -- Ord a => a -> AATree a -> Maybe a
  insert,        -- Ord a => a -> AATree a -> AATree a
  inorder,       -- AATree a -> [a]
  remove,        -- Ord a => a -> AATree a -> AATree a
  size,          -- AATree a -> Int
  height,        -- AATree a -> Int
  checkTree      -- Ord a => AATree a -> Bool
 ) where

--------------------------------------------------------------------------------

-- AA search trees

data AATree a
  = Empty
  | Node  { level :: Int, left :: AATree a, value :: a, right :: AATree a }
  | Leaf  { level :: Int, value :: a }
  deriving (Eq, Show, Read)

-- test trees
--at = Node 0 (Node 1 (Leaf 2 4) 3 (Empty)) 1 (Leaf 0 2)
at = Node 0 (Node 1 (Leaf 2 1) 2 Empty) 3 (Leaf 0 4)
--atbig = (Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Node (Empty) 6 (Leaf 7)) 9 (Leaf 11)))
--leaf = Leaf 1


-- return an empty tree
emptyTree :: AATree a
emptyTree = Empty


-- find an element in the tree
get :: Ord a => a -> AATree a -> Maybe a
get _ Empty = Nothing
get a (Leaf _ x)
  | a == x = Just x
  | otherwise = Nothing
get a (Node _ left y right) = case compare a y of
  LT -> get a left
  EQ -> Just a
  GT -> get a right


-- insert an element
-- auto balance with skew/split
insert :: Ord a => a -> AATree a -> AATree a
insert x Empty = Node 1 Empty x Empty
insert x (Node level left y right)
  | x < y     = balance $ Node level (insert x left) y right
  | otherwise = balance $ Node level left y (insert x right)
  where balance = split . skew


-- skew
skew :: AATree a -> AATree a
skew tree@(Node level _ _ (Node v _ _ _))
  | v == level = rightRotate tree
skew tree = tree


-- split
split :: AATree a -> AATree a
split tree@(Node level _ _ (Node _ _ _ (Node v _ _ _)))
  | v == level = Node (level + 1) left x right
    where (Node _ left x right) = leftRotate tree
split tree = tree


-- left rotation
leftRotate :: AATree a -> AATree a
leftRotate (Node level a p (Node _ b q c)) = Node level (Node level a p b ) q c
leftRotate Empty = emptyTree
leftRotate leaf@(Leaf _ _) = leaf


-- right rotation
rightRotate :: AATree a -> AATree a
rightRotate (Node level (Node _ a p b) q c) = Node level a p (Node level b q c)
rightRotate Empty = emptyTree
rightRotate leaf@(Leaf _ _) = leaf


-- inorder traversal
inorder :: AATree a -> [a]
inorder (Leaf _ a) = [a]
inorder Empty = []
inorder (Node _ left a right) = inorder left ++ [a] ++ inorder right


-- get size of tree (# nodes != Empty)
size :: AATree a -> Int
size Empty = 0
size (Leaf _ _) = 1
size (Node _ left _ right) = 1 + (size left) + (size right)


-- get height of tree recursively
height :: AATree a -> Int
height (Node _ left _ right) = 1 + max (height left) (height right)
height (Leaf _ _) = 1
height Empty = 0

--------------------------------------------------------------------------------
-- Optional function

remove :: Ord a => a -> AATree a -> AATree a
remove = error "remove not implemented"

--------------------------------------------------------------------------------
-- Check that an AA tree is ordered and obeys the AA invariants

checkTree :: Ord a => AATree a -> Bool
checkTree root =
  isSorted (inorder root) &&
  all checkLevels (nodes root)
  where
    nodes x
      | isEmpty x = []
      | otherwise = x:nodes (leftSub x) ++ nodes (rightSub x)


-- True if the given list is ordered
-- zip each element with its successor
-- then check if x <= y
-- e.g. [1,2,3,4] => [(1, 2), (2, 3), (3, 4)]
isSorted :: Ord a => [a] -> Bool
isSorted xs = and $ zipWith (<=) xs (tail xs)


-- Check if the invariant is true for a single AA node
-- You may want to write this as a conjunction e.g.
--   checkLevels node =
--     leftChildOK node &&
--     rightChildOK node &&
--     rightGrandchildOK node
-- where each conjunct checks one aspect of the invariant
checkLevels :: AATree a -> Bool
checkLevels (Node level (Node levellc _ _ _) _ (Node levelrc _ _ (Node levelrgc _ _ _))) = leftChildOK && rightChildOK && rightGrandchildOK
  where
    leftChildOK = levellc == (level + 1)
    rightChildOK = (levelrc == level) || (levelrc == (level + 1))
    rightGrandchildOK = level >= levelrgc
checkLevels (Leaf _ _) = True
checkLevels Empty = True
checkLevels (Node _ Empty _ Empty) = True
checkLevels _ = False


-- check if tree is empty
isEmpty :: AATree a -> Bool
isEmpty a = size a == 0


-- get subtrees
-- we only care about left/right trees hence the wildcards
--
-- get left subtree
leftSub :: AATree a -> AATree a
leftSub (Node _ left _ _) = left

-- get right subtree
rightSub :: AATree a -> AATree a
rightSub (Node _ _ _ right) = right

--------------------------------------------------------------------------------
