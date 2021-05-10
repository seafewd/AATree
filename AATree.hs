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
  deriving (Eq, Show, Read)


-- return an empty tree
emptyTree :: AATree a
emptyTree = Empty


-- find an element in the tree
get :: Ord a => a -> AATree a -> Maybe a
get _ Empty = Nothing
get a (Node _ l y r) = case compare a y of
  LT -> get a l
  EQ -> Just a
  GT -> get a r


-- insert an element
-- auto balance with skew + split at every node
insert :: Ord a => a -> AATree a -> AATree a
insert x Empty = Node 1 Empty x Empty
insert x (Node lvl l y r)
  | x < y = balance $ Node lvl (insert x l) y r
  | x > y = balance $ Node lvl l y (insert x r)
  | otherwise = Node lvl l y r
  where balance = split . skew


-- skew
-- rotate right if root level is the same as the level of its left child
skew :: AATree a -> AATree a
skew tree@(Node plvl (Node lclvl _ _ _) _ _)
  | plvl == lclvl = rotateRight tree
skew tree = tree


-- split
-- rotate left if root level is the same as its right grandchild's level
split :: AATree a -> AATree a
split tree@(Node plvl _ _ (Node _ _ _ (Node rgclvl _ _ _)))
  | rgclvl == plvl = rotateLeft tree
split tree = tree


-- left rotation
rotateLeft :: AATree a -> AATree a
rotateLeft (Node plevel lc pval (Node _ rclc rcval rcrc)) = Node (plevel + 1) (Node plevel lc pval rclc) rcval rcrc
rotateLeft tree = tree


-- right rotation
rotateRight :: AATree a -> AATree a
rotateRight (Node plevel (Node lclevel lclc lcval lcrc) pval rc)
  = Node lclevel lclc lcval (Node plevel lcrc pval rc)
rotateRight p@(Node plevel Empty pval _) = Node plevel Empty pval p
rotateRight tree = tree


-- inorder traversal
inorder :: AATree a -> [a]
inorder Empty = []
inorder (Node _ l a r) = inorder l ++ [a] ++ inorder r


-- get size of tree (# nodes != Empty)
size :: AATree a -> Int
size Empty = 0
size (Node _ l _ r) = 1 + (size l) + (size r)


-- get height of tree recursively
height :: AATree a -> Int
height (Node _ l _ r) = 1 + max (height l) (height r)
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
checkLevels (Node level1 (Node levellc _ _ _) _ (Node levelrc _ _ (Node levelrgc _ _ _))) =
  leftChildOK &&
  rightChildOK &&
  rightGrandchildOK
  where
    leftChildOK = levellc < level1
    rightChildOK = (levelrc <= level1)
    rightGrandchildOK = level1 > levelrgc
checkLevels _ = True


-- check if tree is empty
isEmpty :: AATree a -> Bool
isEmpty a = size a == 0


-- get subtrees
-- we only care about left/right trees hence the wildcards
--
-- get left subtree
leftSub :: AATree a -> AATree a
leftSub = left

-- get right subtree
rightSub :: AATree a -> AATree a
rightSub = right

--------------------------------------------------------------------------------
