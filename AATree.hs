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
  = Empty { level :: Int }
  | Node  { level :: Int, left :: AATree a, value :: a, right :: AATree a }
  | Leaf  { level :: Int, value :: a }
  deriving (Eq, Show, Read)

-- test trees
--at = Node 0 (Node 1 (Leaf 2 4) 3 (Empty)) 1 (Leaf 0 2)
at = Node 0 (Node 1 (Leaf 2 1) 2 (Empty 1)) 3 (Leaf 0 4)
--atbig = (Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Node (Empty) 6 (Leaf 7)) 9 (Leaf 11)))
--leaf = Leaf 1

-- return an empty tree
emptyTree :: AATree a
emptyTree = (Empty 0)

-- find an element in the tree
get :: Ord a => a -> AATree a -> Maybe a
get _ (Empty _) = Nothing
get a (Leaf _ x)
  | a == x = Just x
  | otherwise = Nothing
get a (Node _ left y right) = case compare a y of
  LT -> get a left
  EQ -> Just a
  GT -> get a right

-- insert an element
insert :: Ord a => AATree a -> a -> AATree a
insert (Node n l v r) x
  | x < v = fixup (Node n (insert l x) v r)
  | x > v = fixup (Node n l v (insert r x))
  where fixup = split . skew
insert (Empty _) x = Node 1 (Empty 1) x (Empty 0)

-- helper for insert
-- right rotation
skew :: AATree a -> AATree a
skew (Node n (Node ln ll lv lr) v r)
  | ln == n = Node ln ll lv (Node n lr v r)
skew t = t

split :: AATree a -> AATree a
split (Node tn a tv (Node rn b rv x@(Node xn _ _ _)))
  | tn == xn = Node (rn+1) (Node tn a tv b) rv x
split t = t

-- inorder traversal
inorder :: AATree a -> [a]
inorder (Leaf _ a) = [a]
inorder (Empty _) = []
inorder (Node _ left a right) = inorder left ++ [a] ++ inorder right

-- get size of tree (# nodes != Empty)
size :: AATree a -> Int
size (Empty _) = 0
size (Leaf _ _) = 1
size (Node _ left _ right) = 1 + (size left) + (size right)

-- get height of tree recursively
height :: AATree a -> Int
height (Node _ left _ right) = 1 + max (height left) (height right)
height (Leaf _ _) = 1
height (Empty _) = 0

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
checkLevels = error "checkLevels not implemented"

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


-- show tree
-- shamelessly stolen thank you internet
showTree :: Show a => AATree a -> String
showTree (Empty _) = "Empty root."
showTree (Node level left value right) =
  unlines (ppHelper (Node level left value right))
    where
      pad :: String -> String -> [String] -> [String]
      pad first rest =
        zipWith (++) (first : repeat rest)

      ppSubtree :: Show a => AATree a -> AATree a -> [String]
      ppSubtree left right =
        pad "+- " "|  " (ppHelper left) ++ pad "`- " "   " (ppHelper right)

      ppHelper :: Show a => AATree a -> [String]
      ppHelper (Empty _) = []
      ppHelper (Node level left value right) =
        (show value) : ppSubtree left right

--------------------------------------------------------------------------------
