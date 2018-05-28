import Prelude hiding 
                    (foldr,
                    sum,
                    product,
                    or,
                    and, 
                    length, 
                    snoc,
                    reverse,
                    concat                    
                    )

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f v []     = v 
foldr f v (x:xs) = x `f` (foldr f v xs)

sum :: Num a => [a] -> a
sum = foldr (+) 0

product :: Num a => [a] -> a 
product = foldr (*) 1

or :: [Bool] -> Bool
or = foldr (||) False

and :: [Bool] -> Bool
and = foldr (&&) True

concat :: [[a]] -> [a]
concat = foldr (++) []

snoc :: a -> [a] -> [a]
snoc x ws = ws ++ [x]

reverse :: [a] -> [a]
reverse = foldr (snoc) [] 

length :: Integral i => [a] -> i
length = foldr (\x y -> y+1) 0


