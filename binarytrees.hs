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


-- Root Left Right
preorder :: BTree a -> [a]
preorder Leaf = []
preorder (Node l a r) = a : preorder l ++ preorder r

-- Left Root Right
inorder :: BTree a ->  [a]
inorder Leaf = []
inorder (Node l a r) =  inorder l ++ [a] ++ inorder r

-- Left Right Root
postorder :: BTree a -> [a]
postorder Leaf = []
postorder (Node l a r) = postorder l ++ postorder r ++ [a]



-- Testing Tree

testTree :: BTree Integer
testTree =
  Node (Node Leaf 3 Leaf)
  1
  (Node Leaf 4 Leaf)
  
  
