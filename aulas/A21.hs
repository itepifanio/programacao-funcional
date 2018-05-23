module Trees where

data Tree a = Leaf a | Node (Tree a) a (Tree a)
    deriving (Eq, Show)

flatten :: Tree a -> [a]
flatten (Leaf x)     = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

tmap :: (a -> b) -> Tree a -> Tree b
tmap f (Leaf x)     = Leaf (f x)
tmap f (Node l x r) = Node (tmap f l) (f x) (tmap f r)
