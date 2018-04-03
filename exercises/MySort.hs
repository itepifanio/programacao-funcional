module MySort
    ( sort
    , msort
    , isort
    , qsort
    ) where

import Prelude hiding ( sort )

sort :: Ord a => [a] -> [a]
sort = msort

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort ys) (msort zs)
    where (ys, zs) = halve xs

halve :: [a] -> ([a],[a])
halve xs = splitAt n xs
    where n = length xs `div` 2

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <= y     = x : merge xs (y:ys)
    | otherwise  = y : merge (x:xs) ys 


isort :: Ord a => [a] -> [a]
isort []     = []
isort (x:xs) = insert x (isort xs)

insert :: Ord => a -> [a] -> [a]
insert w []
insert w (x:xs)
    | w <= x    = w : x : xs
    | otherwise = x : insert w xs

qsort :: Ord a => [a] -> [a]
qsort []     = undefined
qsort (x:xs) = undefined

