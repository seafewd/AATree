{-# LANGUAGE TemplateHaskell #-}

module TwoThree where

import Data.List (nub, sort)
import Data.Maybe
import qualified Data.Tree as T
import Test.QuickCheck

data Tree a 
  = Empty
  | Two   (Tree a) a (Tree a)
  | Three (Tree a) a (Tree a) a (Tree a)
  | Four  (Tree a) a (Tree a) a (Tree a) a (Tree a)
  deriving (Show, Eq)

-- Smart constructors

two :: a -> Tree a
two x = Two Empty x Empty

three :: a -> a -> Tree a
three x y = Three Empty x Empty y Empty

four :: a -> a -> a -> Tree a
four x y z = Four Empty x Empty y Empty z Empty

instance Functor Tree where
  fmap f t = case t of
    Empty           -> Empty
    Two l x r       -> Two (fmap f l) (f x) (fmap f r)
    Three l x m y r -> Three (fmap f l) (f x) (fmap f m) (f y) (fmap f r)
    Four{}          -> error "four"

instance Foldable Tree where
  foldMap f = rec 
   where 
    rec t = let (#) = mappend in case t of
      Empty           -> mempty
      Two l x r       -> rec l # f x # rec r
      Three l x m y r -> rec l # f x # rec m # f y # rec r
      Four{}          -> error "four"

get :: Ord a => a -> Tree a -> Maybe a
get _ Empty = Nothing
get x (Two l y r) = case compare x y of
  LT -> get x l
  EQ -> Just y
  GT -> get x r
get x (Three l y m z r) = case (compare x y, compare x z) of
  (LT, _)  -> get x l
  (EQ, _)  -> Just y
  (GT, LT) -> get x m
  (_, EQ)  -> Just z
  (_, GT)  -> get x r
get _ Four{} = error "four"

insert :: Ord a => a -> Tree a -> Tree a
insert x = absorb . insert' x

insert' :: Ord a => a -> Tree a -> Tree a
insert' x Empty = Two Empty x Empty
insert' x (Two l y r) = absorb $ case compare x y of
  LT -> Two (insert' x l) y r
  EQ -> Two l x r
  GT -> Two l y (insert' x r)
insert' x (Three l y m z r) = absorb $ case (compare x y, compare x z) of
  (LT, _)  -> Three (insert' x l) y m z r
  (EQ, _)  -> Three l x m z r
  (GT, LT) -> Three l y (insert' x m) z r
  (_, EQ)  -> Three l y m x r
  (_, GT)  -> Three l y m z (insert' x r)
insert' _ Four{} = error "four"

absorb :: Tree a -> Tree a
absorb t = case t of 
  -- Two node parent
  Two Empty x (Two l y r) -> Three Empty x Empty y Empty
  Two (Two _ x _) y Empty -> Three Empty x Empty y Empty
  -- Three node parent
  Three (Two _ x _) y Empty z Empty -> four x y z 
  Three Empty x (Two _ y _) z Empty -> four x y z
  Three Empty x Empty y (Two _ z _) -> four x y z
  -- Split and absorb
  Two (Four t1 a t2 b t3 c t4) x r  -> Three (Two t1 a t2) b (Two t3 c t4) x r
  Two l x (Four t1 a t2 b t3 c t4)  -> Three l x (Two t1 a t2) b (Two t3 c t4)  
  -- Split and push upwards
  Three (Four t1 a t2 b t3 c t4) x m y r -> Four (Two t1 a t2) b (Two t3 c t4) x m y r 
  Three l x (Four t1 a t2 b t3 c t4) y r -> Four l x (Two t1 a t2) b (Two t3 c t4) y r 
  Three l x m y (Four t1 a t2 b t3 c t4) -> Four l x m y (Two t1 a t2) b (Two t3 c t4) 
  -- New root
  Four t1 a t2 b t3 c t4 -> Two (Two t1 a t2) b (Two t3 c t4)
  _ -> t 

size :: Tree a -> Int
size = foldr (const (+1)) 0 

fromList :: Ord a => [a] -> Tree a
fromList = foldr insert Empty

toList :: Tree a -> [a]
toList = foldr (:) [] 

height :: Tree a -> Int
height Empty             = -1
height (Two l _ r)       = 1 + max (height l) (height r)
height (Three l _ m _ r) = 1 + maximum (map height [l, m, r])

-- Test

instance (Ord a, Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = arbitrary >>= return . fromList

prop_size :: [Int] -> Property
prop_size xs = length (nub xs) === size (fromList xs)

prop_elements :: [Int] -> Property
prop_elements xs = sort (nub xs) === toList (fromList xs)

prop_balance :: [Int] -> Bool
prop_balance = inv . fromList

inv :: Tree a -> Bool
inv t = case t of
  Empty           -> True
  Two l x r       -> height l == height r && inv l && inv r
  Three l x m y r -> height l == height m && height m == height r && 
                     inv l && inv m && inv r

prop_insert :: [Int] -> Bool
prop_insert [] = True
prop_insert xs@(x:_) = isJust $ get x (fromList xs) 

toDataTree :: Show a => Tree a -> T.Tree String
toDataTree Empty             = T.Node "⊥" []
toDataTree (Two l x r)       = T.Node (show x) $ map toDataTree [r, l]
toDataTree (Three l x m y r) = T.Node (show (x, y)) $ map toDataTree [r, m, l]

printTree :: Show a => Tree a -> IO ()
printTree = putStrLn . T.drawTree . toDataTree

return []
runTests = $quickCheckAll
