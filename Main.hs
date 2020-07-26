module Main where
import BalancedTree
import Test.QuickCheck
import Tree hiding (insTree, delete)

main = quickCheck prop_height


prop_height_del :: [Integer] -> Integer -> Bool
prop_height_del [] _  = True
prop_height_del (x1:x2:xs) y
 = fromInteger (height tree) <= avl_height_max
   where
   tree = delete y (foldr insTree Nil (x1:x2:xs))
   avl_height_max = 1.44 * (log (fromInteger (size tree)) / log 2) + 1
prop_height_del (x:xs) _ = True

prop_height :: [Integer] -> Bool
prop_height [] = True -- an empty tree is balanced
prop_height (x:xs) 
 = fromInteger (height tree) <= avl_height_max
   where
   tree = foldr insTree Nil (x:xs)
   avl_height_max = 1.44 * (log (fromInteger (size tree)) / log 2) + 1
