import Prelude hiding(replicate)

-- 1 - Soma dos primeiros 100 quadrados inteiros
soma :: Integer
soma = sum [x^2|x<-[1..100]]

-- 2 - funcao replicate
replicate :: Int -> a -> [a]
replicate n tipo = [tipo| _ <-[1..n]]

-- 3 - pythagorean if x^2 + y^2 = z^2
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z)| x <- ns, y <- ns, z <- ns, x^2 + y^2 == z^2]
                 where 
                      ns = [1..n]
-- 4)
factors :: Int -> [Int]
factors n = [x|x<-[1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [n|n <- [1..n], n == sum (init (factors n))]

-- 5)
comprehension =  concat [[(x, y) | y <- [4, 5, 6]] | x <- [1, 2, 3]]

-- 6) 
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..n], x == x']
    where n = length xs - 1

find f xs = [v | (f', v) <- xs, f == f']
generateTuple xs = zip xs [0..n]
    where n = length xs - 1

positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = find x (generateTuple xs)

-- 7)
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]
