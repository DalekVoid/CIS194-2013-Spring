import Data.List
import Data.Foldable

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

instance Functor Tree where
  fmap f Leaf                    = Leaf
  fmap f (Node n leftT x rightT) = Node n (fmap f leftT) (f x) (fmap f rightT) 

instance Foldable Tree where
  foldr f initial Leaf = 
  foldr f initial t@(Node n leftT x rightT) = 
  
size :: Tree a -> Integer
height :: Tree a -> Integer
height Leaf = -1
height Node h _ _ _ = 1 + h

treeAppend :: Tree a -> a -> Tree a
treeAppend Leaf x = Node 0 Leaf x Leaf
treeAppend (Node Integer t1 _ t2) x
  | height t1 > height t2 = 

foldTree :: [a] -> Tree a
foldTree = foldl' treeAppend Leaf
