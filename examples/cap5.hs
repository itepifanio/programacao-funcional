import Prelude hiding (concat, length)

-- Uma funcao que concatena uma 
-- lista de listas utilizando um
-- gerador (generator)

concat     :: [[a]] -> [a]
concat xss =  [x|xs <- xss, x <- xs]

-- firsts é uma funcao que seleciona
-- os primeiros componentes de uma 
-- lista de pares

firsts    :: [(a,b)] -> [a]
firsts ps =  [x|(x,_) <- ps]

-- Implementando funcao length
-- Obs:. Nesse caso o gerador _ <- xs
-- serve simplesmente como contador

length :: [a] -> Int
length xs = sum[1|_<-xs]

-- Guards
-- a funcao factors calcula os divisores 
-- de um numero n

factors   :: Int -> [Int]
factors n =  [x|x <- [1..n], n `mod` x == 0]

-- Definindo se um numero é primo

prime   :: Int -> Bool
prime n =  factors n == [1,n]

-- Lista os numeros primos até n

primes :: Int -> [Int]
primes n = [x|x <- [2..n], prime x]
