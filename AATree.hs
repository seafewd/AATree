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

-- test trees
--at = Node 0 (Node 1 (Leaf 2 4) 3 (Empty)) 1 (Leaf 0 2)
at1 = Node 1 (Node 2 (Node 3 Empty 1 Empty) 2 Empty) 3 (Node 2 Empty 4 Empty)
at2 = Node 1 (Node 2 Empty 1 Empty) 2 (Node 2 Empty 3 Empty)
at3 = Node 1 Empty 1 Empty
--atbig = (Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Node (Empty) 6 (Leaf 7)) 9 (Leaf 11)))
--leaf = Leaf 1


-- return an empty tree
emptyTree :: AATree a
emptyTree = Empty


-- find an element in the tree
get :: Ord a => a -> AATree a -> Maybe a
get _ Empty = Nothing
get a (Node _ left y right) = case compare a y of
  LT -> get a left
  EQ -> Just a
  GT -> get a right


-- insert an element
-- auto balance with skew + split at every node
{-
insert :: Ord a => a -> AATree a -> AATree a
insert x Empty = Node 1 Empty x Empty
insert x (Node level left y right)
  | x < y     = balance $ Node level (insert x left) y right
  | otherwise = balance $ Node level left y (insert x right)
  where balance = split . skew
-}
insert x Empty = Node 1 Empty x Empty
insert x (Node lv l y r) = case compare x y of
   LT -> Node lv (insert x l) y r .$ skew .$ split
   GT -> Node lv l y (insert x r) .$ skew .$ split
   EQ -> Node lv l x r

(.$) :: a -> (a -> b) -> b
(.$) = flip ($)

-- | right rotation
skew :: AATree a -> AATree a
skew t@(Node lvT l@(Node lvL a _ b) _ r)
  | lvT == lvL = let t' = t {left = right l}
                     l' = l {right = t'}
                 in l'

  | otherwise = t
skew t = t

-- | left rotation and level increase
split :: AATree a -> AATree a
split t@(Node lvT a _ r@(Node lvR b _ x@(Node lvX _ _ _)))
  | lvT == lvX = let t' = t {right = left r}
                     r' = r {left = t', level = lvR + 1}
                 in r'
  | otherwise = t
split t = t

{-
-- skew
skew :: AATree a -> AATree a
skew tree@(Node l1 (Node l2 _ _ _) _ _)
  | l2 == l1 = rightRotate tree
skew tree = tree


-- split
split :: AATree a -> AATree a
split tree@(Node level _ _ (Node _ _ _ (Node v _ _ _)))
  | v == level = Node (level + 1) left x right
    where (Node _ left x right) = leftRotate tree
split tree = tree
-}

-- left rotation
leftRotate :: AATree a -> AATree a
leftRotate (Node level a p (Node _ b q c)) = Node level (Node level a p b) q c
leftRotate Empty = emptyTree


-- right rotation
rightRotate :: AATree a -> AATree a
rightRotate (Node level (Node _ a p b) q c) = Node level a p (Node level b q c)
rightRotate Empty = emptyTree

-- increment level of a tree
incLevel :: AATree a -> AATree a
incLevel (Node level left value right) = Node (level + 1) left value right


-- inorder traversal
inorder :: AATree a -> [a]
inorder Empty = []
inorder (Node _ left a right) = inorder left ++ [a] ++ inorder right


-- get size of tree (# nodes != Empty)
size :: AATree a -> Int
size Empty = 0
size (Node _ left _ right) = 1 + (size left) + (size right)


-- get height of tree recursively
height :: AATree a -> Int
height (Node _ left _ right) = 1 + max (height left) (height right)
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
checkLevels (Node level (Node levellc _ _ _) _ (Node levelrc _ _ (Node levelrgc _ _ _))) =
  leftChildOK &&
  rightChildOK &&
  rightGrandchildOK
  where
    leftChildOK = levellc == (level + 1)
    rightChildOK = (levelrc == level) || (levelrc == (level + 1))
    rightGrandchildOK = level > levelrgc
checkLevels _ = True
{-
checkLevels (Node _ Empty _ Empty) = True
checkLevels (Node level1 node@(Node level2 _ _ _) _ Empty) = level1 < level2
checkLevels (Node level1 Empty _ node@(Node level2 _ _ _)) = level1 < level2
-}


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
