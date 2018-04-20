module A15 where

ones :: [Integer]

ones = [1,1..]

ones' = concat . repeat

ones'' = 1 : ones''


