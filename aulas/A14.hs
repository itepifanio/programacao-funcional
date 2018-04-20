module A14 where

import Prelude hiding ( until )

until :: (a -> Bool) -> (a -> a) -> a -> a
until b p i = if b i
              then i
              else until b p (p i)
 
sorted' :: Ord a => [a] -> Bool
sorted' []  = True
sorted' [x] = True
sorted' (x:y:xs)
    | x <= y    = sorted(y:xs)
    | otherwise = False

-- sorted (x:y:xs) = x <= y && sorted (y:xs)


