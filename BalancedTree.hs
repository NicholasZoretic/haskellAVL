-- AVL tree
module BalancedTree where
import Tree hiding (insTree, delete, delHelp)

-- height is the # of nodes rather than edges
height :: Tree a -> Integer
height Nil = 0
height (Node x t1 t2) = 1 + (max (height t1) (height t2))

balanceFactor :: Tree a -> Integer
balanceFactor t = height (rightSub t) - height (leftSub t)

-- rotate right
rotateR :: Tree a -> Tree a
rotateR (Node x (Node y a b) c) = (Node y a (Node x b c))

-- rotate left
rotateL :: Tree a -> Tree a
rotateL (Node x a (Node y b c)) = (Node y (Node x a b) c)

insTree :: Ord a => a -> Tree a -> Tree a
insTree a Nil = Node a Nil Nil
insTree a (Node b t1 t2)
 | a < b  = balance (Node b (insTree a t1) t2)
 | a > b  = balance (Node b t1 (insTree a t2))
 | a == b = Node b t1 t2

delete :: Ord a => a -> Tree a -> Tree a
delete a Nil = Node a Nil Nil
delete a (Node b t1 t2)
 | a < b  = balance (Node b (delete a t1) t2)
 | a > b  = balance (Node b t1 (delete a t2))
 | a == b = delHelp a (Node b t1 t2)

delHelp :: Ord a => a -> Tree a -> Tree a
delHelp a (Node b t1 t2)
 | isNode t2 = Node mn t1 (delete mn t2)
 | isNode t1 = Node mx (delete mx t1) t2
 | otherwise = Nil
   where
   mn = minTree t2
   mx = maxTree t1

balance :: Tree a -> Tree a
balance t@(Node x lt rt)
 | (bf == 2) && (rbf == 1)       = rotateL t
 | (bf == 2) && (rbf == (-1))    = rotateL (Node x lt (rotateR rt))
 | (bf == (-2)) && (lbf == 1)    = rotateR (Node x (rotateL lt) rt)
 | (bf == (-2)) && (lbf == (-1)) = rotateR t
 | otherwise                     = t
   where
   bf  = balanceFactor t
   lbf = balanceFactor lt
   rbf = balanceFactor rt

