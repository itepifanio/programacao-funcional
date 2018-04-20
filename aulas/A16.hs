-- Conteudos pra prova:

-- isInfixOf
-- map, filter, zip, zipWith
-- inits, tails, subsequences
-- length
-- curry, uncurry, flip
-- (+), (*), (^)
-- definir naturais e listas
-- data Nat = ?


-- Nova definição de nats
nats :: Integral i => [i]
nats = 0 : map (+1) nats

fibs :: Integral i => [i]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

cross :: (a -> b, c -> d) -> (a, c) -> (b, d)
cross (f, g) (x,y) = (f x, g y)

pair :: (a -> b, a -> c) -> a -> (b, c)
pair (f,g) x = (f x, g x) 



