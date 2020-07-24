module Main where
import BalancedTree
import Test.QuickCheck
import Tree hiding (insTree, delete)

main = quickCheck prop_height



prop_height :: [Integer] -> Bool
prop_height [] = True -- an empty tree is balanced
prop_height (x:xs) 
 = fromInteger (height tree) <= avl_height_max
   where
   tree = foldr insTree Nil (x:xs)
   avl_height_max = 1.44 * (log (fromInteger (size tree)) / log 2) + 1
