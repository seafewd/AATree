{-
  to run:
  $ ghc -e main Main.hs < swahili-small.txt

  to compile and run:
  $ ghc -O Main.hs && ./Main < swahili-small.txt
-}

import AATree
import Data.Traversable
import Data.List ( permutations, foldl', sort )

--------------------------------------------------------------------------------

main :: IO ()
main = do
  -- get contents of file
  contents <- getContents
  -- split the data into words
  let list = words contents
  -- build an AA tree using the list
  let fullTree = buildTree list

  -- calculate statistics
  let treeSize = size fullTree
  let treeHeight = height fullTree
  let optimalHeight = ((ceiling . logBase 2.0 . fromIntegral) (treeSize + 1)) - 1
  let ratio = (fromIntegral treeHeight) / (fromIntegral optimalHeight)
  let checkTreeStatus = checkTree fullTree

  -- print statistics
  print ("Size: " ++ show treeSize)
  print ("Height: " ++ show treeHeight)
  print ("Optimal height: " ++ show optimalHeight)
  print ("Height / Optimal height: " ++ show ratio)
  print ("checkTree: " ++ show checkTreeStatus)
  print ("First 20 words: " ++ (printFirstn 20 fullTree))


-- print the first 20 elements from the list (inorder traversal)
printFirstn :: Int -> AATree String -> String
printFirstn n tree = unwords $ take n $ inorder tree


-- build a tree from a list
buildTree :: Ord a => [a] -> AATree a
buildTree [] = emptyTree
buildTree list = foldl (flip insert) emptyTree (tail list)
--------------------------------------------------------------------------------
