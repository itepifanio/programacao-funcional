-- 1) find the last element of a list
myLast :: [a] -> a
myLast [x]    = x 
myLast (x:xs) = myLast xs

-- 2) Find the last but one element of a list
myButLast :: [a] -> a
myButLast [y,x]  = y
myButLast (x:xs) = myButLast xs

{-- 3) Find the K'th element of a list. 
The first element in the list is the number 1 --}
elementAt :: [a] -> Int -> a
elementAt (x:_) 1  = x
elementAt (_:xs) n = elementAt xs (n-1)

-- 4) Find the number of elements of a list
myLength :: [a] -> Int
myLength []     = 0
myLength (x:xs) = 1 + myLength xs

-- 5) Reverse a list
myReverse :: [a] -> [a]
myReverse [x]    = [x]
myReverse (x:xs) = myReverse xs ++ [x]

{-- 6) Find out whether a list is a palindrome. 
A palindrome can be read forward or backward --}
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = if (xs == (myReverse xs)) then True else False

-- 7)  Flatten a nested list structure
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x
