module BinaryTree where

data BTree a =
    Leaf
  | Node (BTree a) a (BTree a)
  deriving (Eq, Ord, Show)


insert' :: Ord a
        => a
        -> BTree a
        -> BTree a

insert' value Leaf = Node Leaf value Leaf
insert' value (Node left a right)
  | value == a = Node left a right 
  | value < a  = Node (insert' value left) a right
  | value > a  = Node left a (insert' value right)


mapTree :: (a -> b)
        -> BTree a
        -> BTree b

mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)


-- Testing

testTree :: BTree Integer
testTree =
  Node (Node Leaf 3 Leaf)
  1
  (Node Leaf 4 Leaf)
  
  
