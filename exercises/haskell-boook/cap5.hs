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
