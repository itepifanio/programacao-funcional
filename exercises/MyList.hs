module ExList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C


head :: [a] -> a
head []    = error "head of empty list"
head (x:_) = x

tail :: [a] -> [a]
tail []     = error "tail of empty list"
tail (_:xs) = xs

null :: [a] -> Bool
null [] = True
null _  = False

length :: Integral i => [a] -> i
length []     = 0
length (_:xs) = 1 + length xs

sum :: Num a => [a] -> a
sum []     = 0
sum (x:xs) = x + sum xs

product :: Num a => [a] -> a
product []     = 1
product (x:xs) = x * product xs

reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc w []     = [w]
snoc w (x:xs) = x : snoc w xs

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

minimum :: Ord a => [a] -> a
minimum []  = error "minimum of empty list"
minimum [x] = x
minimum (x:xs)
  | x < m     = x
  | otherwise = m
  where m = minimum xs

maximum :: Ord a => [a] -> a
maximum []     = error "maximum of empty list"
maximum [x]    = x
maximum (x:xs) =
    let m = maximum xs
    in if x > m then x else m

take :: Integral i => i -> [a] -> [a]
take _  []    = []
take 0  _     = []
take n (x:xs) = x : take (n - 1) xs

drop :: Integral i => i -> [a] -> [a]
drop n []     = []
drop 0 list   = list
drop n (x:xs) = drop (n-1) xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ []     = []
takeWhile b (x:xs) = x : if b x 
                         then takeWhile b xs
                         else []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ []     = []
dropWhile b (x:xs) = if b x 
                     then dropWhile b xs
                     else xs

tails :: [a] -> [[a]]
tails []         = [[]]
tails all@(x:xs) = all:(tails xs)

init :: [a] -> [a]
init []     = []
init [x]    = []
init (x:xs) = x : init xs

inits :: [a] -> [[a]]
inits []   = []
inits xs   = init xs : inits (init xs)

any :: (a -> Bool) -> [a] -> Bool
any b []     = False
any b (x:xs) = if b x
               then True
               else any b xs

all :: (a -> Bool) -> [a] -> Bool
all b [x] = if b x then True else False
all b (x:xs) = if b x
               then all b xs
               else False

and :: Bool -> Bool -> Bool
and x y = if x == True 
          then if y == True
               then True
               else False
          else False     

or :: Bool -> Bool -> Bool
or x y = if x == True
         then if y == True
              then False
              else True
         else True

concat :: [[a]] -> [a]
concat []       = []
concat ([]:xss) = concat xss
concat ((x:xs):xss) = x: concat ((xs):xss)

elem :: Eq a => a -> [a] -> Bool
elem n [] = False
elem n (x:xs) = if n == x 
                then True 
                else elem n xs

-- Esta pegando os index 
-- acima de 1 de forma errada
(!!) :: [a] -> Int -> a
(!!) (x:xs) 0 = x
(!!) (x:xs) n = if length (x:xs) == n
                then x
                else (!!) xs n

-- filter
-- map
-- cycle
-- repeat
-- replicate

-- isPrefixOf
-- isInfixOf
-- isSuffixOf

-- zip
-- zipWith

-- intercalate
-- nub

-- splitAt
-- break

-- lines
-- words
-- unlines
-- unwords

-- transpose

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome = undefined

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

