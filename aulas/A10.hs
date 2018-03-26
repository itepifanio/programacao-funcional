module A10 where

-- -- {{{ Shapes
data Shape =  Circle Double
            | Rectangle Double Double
    deriving ( Show, Eq )          
-- -- }}}

area :: Shape -> Double
area (Circle r) = pi * r^2
area (Rectangle w h) = w * h

name :: Shape -> String
name s = case s of
            Circle r -> "Its a circle"
            Rectangle w h -> "Its a rectangle"

-- Um case
data Person = Amigo String
            | Colega String String
            deriving (Show, Eq)

father :: Person -> Person
father (Amigo "Thanos") = nikos

thanos = Amigo "Thanos"
nikos  = Colega "Nikos" "Tsounas"

friendWithDad :: Person -> Bool
friendWithDad p = case father p of
                    Amigo _    -> True
                    Colega _ _ -> False

-- Define list

--data ListInt = EmptyInt | ConsInt Integer :+ ListInt
--    deriving (Show, Eq)

data List a = Empty | Cons a (List a)
    deriving (Show, Eq)

--(:+) = Cons
--infix 5 :+

--data [] a = [] | (:) a ([] a)

-- 1) data [a] = [] | a : []

myHead :: [a] -> a
myHead []    = error "não tem cabeça"
-- Por conta da definição 1)
--head (x:xs) = x
myHead (x:_) = x

myNull :: [a] -> Bool
myNull [] = True
myNull _  = False

myTail :: [a] -> [a]
myTail [] = error "não tem rabo"
myTail (_:xs) = xs

myLength :: Integral b => [a] -> b
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

mySum :: Num a => [a] -> a
mySum []     = 0
mySum (x:xs) = x + mySum xs

myProduct :: Num a => [a] -> a
myProduct [] = 1
myProduct (x:xs) = x * myProduct xs

myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]
