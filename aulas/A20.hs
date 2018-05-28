module Trees where

data TwoTree a b = Leaf' a | TwoTree (TwoTree a b) b (TwoTree a b)

data Tree a = Leaf a | Tree (Tree a) a (Tree a)
    deriving (Eq, Show)

