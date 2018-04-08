module MyList where

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

infixl 5 ++

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

(!!) :: [a] -> Int -> a
(!!) (x:_) 0 = x
(!!) (_:xs) n = (!!) xs (n-1)

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter b (x:xs) | b x = x : filter b xs
                | otherwise = filter b xs
                  
map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs

cycle :: [a] -> [a]
cycle []     = []
cycle (x:xs) = (x:xs) ++  cycle (x:xs)

repeat :: a -> [a]
repeat n = n : repeat n

replicate :: Int -> a -> [a]
replicate x n = take x (repeat n)

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf _ []          = False
isPrefixOf [] _          = True
isPrefixOf (x:xs) (y:ys) = if x == y
                           then isPrefixOf xs ys
                           else False

--isInfixOf :: Eq a => [a] -> [a] -> Bool
--isInfixOf _ [] = True
--isInfixOf [] _ = False
--isInfixOf (x:xs) (y:ys)
--    | x == y = isInfixOf xs ys
--    | otherwise = isInfixOf xs ys


-- isSuffixOf

zip :: [a] -> [b] -> [(a, b)]
zip [] _          = []
zip _ []          = []
zip (x:xs) (y:ys) = (x, y) : zip xs ys

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
zipWith _ _ _           = []

intercalate :: [a] -> [[a]] -> [a]
intercalate [] _     = []
intercalate x [y]    = y
intercalate x (y:ys) = y ++ x ++ (intercalate x ys)

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = if x `elem` xs
             then nub xs
             else x : nub xs 

splitAt :: Integer -> [a] -> ([a],[a])
splitAt 0 xs = ([], xs)
splitAt n [] = ([],[])
splitAt n (x:xs) = (x: ys , zs)
    where
        (ys, zs) = splitAt (n-1) xs

break :: (a -> Bool) -> [a] -> ([a], [a])
break _ all@[] = (all,all)
break b all@(x:xs)
    | not (b x) = ([], all)
    | otherwise = let (ys, zs) = break b xs 
                  in (x:ys,zs)

lines :: String -> [String]
lines = undefined

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


