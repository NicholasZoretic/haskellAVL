module Tree where

data Tree a = Nil | Node a (Tree a) (Tree a)
              deriving (Eq, Ord, Show, Read)

nil :: Tree a
nil = Nil

isNil :: Tree a -> Bool
isNil (Nil) = True
isNil _     = False

isNode :: Tree a -> Bool
isNode x = not (isNil x)

leftSub :: Tree a -> Tree a
leftSub (Node a t1 t2) = t1
leftSub Nil            = error "leftSub"

rightSub :: Tree a -> Tree a
rightSub (Node a t1 t2) = t2
rightSub Nil            = error "rightSub"

treeVal :: Tree a -> a
treeVal (Node a t1 t2) = a
treeVal Nil            = error "treeVal"

insTree :: Ord a => a -> Tree a -> Tree a

-- assumes no duplicates
insTree a Nil = Node a Nil Nil
insTree a (Node b t1 t2)
 | a < b  = Node b (insTree a t1) t2
 | a > b  = Node b t1 (insTree a t2)
 | a == b = Node b t1 t2

delete :: Ord a => a -> Tree a -> Tree a
delete a Nil = Nil
delete a (Node b t1 t2)
 | a < b  = Node b (delete a t1) t2
 | a > b  = Node b t1 (delete a t2)
 | a == b = delHelp a (Node b t1 t2)

delHelp :: Ord a => a -> Tree a -> Tree a
delHelp a (Node b t1 t2)
 | isNode t2 = Node mn t1 (delete mn t2)
 | isNode t1 = Node mx (delete mx t1) t2
 | otherwise = Nil
   where
   mn = minTree t2
   mx = maxTree t1

minTree :: Tree a -> a
minTree (Node a t1 t2)
 | isNode t1 = minTree t1
 | otherwise = a

maxTree :: Tree a -> a
maxTree (Node a t1 t2)
 | isNode t2 = maxTree t2
 | otherwise = a

size :: Tree a -> Integer
size t
 | isNil t   = 0
 | otherwise = 1 + size (leftSub t) + size (rightSub t)
