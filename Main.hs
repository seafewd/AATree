{-
  to run:
  $ ghc -e main Main.hs < swahili-small.txt

  to compile and run:
  $ ghc -O Main.hs && ./Main < swahili-small.txt
-}

import AATree

--------------------------------------------------------------------------------

main :: IO ()
main = do
  -- get contents of file
  contents <- getContents
  -- split the data into words
  let list = words contents
  -- build an AA tree using the list
  let fullTree = buildTree list
  -- calculate and print statistics
  let treeSize = size fullTree
  let treeHeight = height fullTree
  let optimalHeight = ((ceiling . logBase 2.0 . fromIntegral) (treeSize + 1)) - 1
  let ratio = treeHeight `div` optimalHeight
  let checkTreeStatus = checkTree fullTree


  print ("Size: " ++ show treeSize)
  print ("Height: " ++ show treeHeight)
  print ("Optimal height: " ++ show optimalHeight)
  print ("Height / Optimal height: " ++ show ratio)
  print ("checkTree: " ++ show checkTreeStatus)
  print ("First 20 words: " ++ "WEEERDS")


  -- use fromIntegral/ceiling/logBase

buildTree :: Ord a => [a] -> AATree a
buildTree [] = emptyTree
buildTree (x:xs) = insert x (buildTree xs)

{- ???????????????
buildTree' :: Ord a => [a] -> AATree a
buildTree' list = foldl insert emptyTree list
-}
--------------------------------------------------------------------------------
