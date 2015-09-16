
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

data MTree a = MNode a (Maybe (MTree a)) (Maybe (MTree a))
               deriving (Show)


treeHeight :: Tree a -> Int
treeHeight Empty          = 0
treeHeight (Node _ c1 c2) = 1 + (maximum [(treeHeight c1), (treeHeight c2)])