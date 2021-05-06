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
  let ratio = treeHeight/optimalHeight
  let checkTreeStatus = checkTree fullTree

  print "Size: " + treeSize + "\nHeight: " + treeHeight + "\nOptimal height: " + optimalHeight + "\nHeight / Optimal height: " + ratio + "\ncheckTree: " + checkTreeStatus + "\nFirst 20 words: " + "WEEERDS"
  -- use fromIntegral/ceiling/logBase


buildTree :: [a] -> AATree a
buildTree [] tree = tree
buildTree (x:xs) tree = foldl insert (head xs) tree

--------------------------------------------------------------------------------
