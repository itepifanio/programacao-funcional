inserts :: a -> [a] -> [[a]]
inserts y [] = [[y]]
inserts y all@(x:xs) = (y:all) : map (x:) (inserts y xs)


--insertAt :: Integral i => a -> i -> [a] -> [a]
--insertAt a i []     = [a]
--insertAt a 0 [x]    = [a,x]
--insertAt a i (x:xs) = x : (insertAt a (i-1) xs)
insertAt c n xs = take n xs ++ [c] ++ drop n xs
insertAt'' c n xs = before ++ [c] ++ after
    where (before, after) = splitAt n xs

insert' :: a -> [a] -> [a]
insert' a xs = insertAt a 0 xs

inserts' c xs = [insertAt c i xs | i <- [0 .. length xs]]

cp :: [[a]] -> [[a]]
cp []       = [[]]
--cp [[]]     = []
cp (xs:xss) = [x:ys | x <- xs, ys <- cp xss]

{-
take n xs ++ drop n xs = xs
take m . drop n = drop n . take (m+n)
take m . take n = take (m `min` n)
drop m . drop n = drop (m + n)

map g . map f = map (g . f)
-}

