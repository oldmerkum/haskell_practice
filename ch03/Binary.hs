data Tree a = Node a (Tree a) (Tree a)
    | Empty
    deriving (Show)

simpleTree = Node "parent" (Node "left child" Empty Empty)
    (Node "right child" Empty Empty)

data Child a = Child a
    | Leaf

data HwTree a = HwNode a (Child (HwTree a)) (Child (HwTree a))
