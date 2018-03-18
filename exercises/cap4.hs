import Prelude hiding ((^),(^^))
-- Exercise 1
halve :: [a] -> ([a],[a])
halve lista = (take ((length lista) `div` 2) lista, drop ((length lista) `div` 2) lista)

-- Exercise 2 
-- a) conditional expression
safetailA :: [a] -> [a]
safetailA lista = if length lista == 0 
                  then [] 
                  else drop 1 lista

-- b) Guarder equations
safetailB :: [a] -> [a]
safetailB lista 
    | length lista == 0 = []
    | otherwise = drop 1 lista

-- c) pattern mathing
safetailC :: [a] -> [a]
safetailC (_:fimLista) = fimLista
safetailC [] = []

-- Exercise 3
(/^)              :: Bool -> Bool -> Bool
False /^ False    = False
_     /^ _        = True


-- (/^) :: Bool -> Bool -> Bool
-- False /^ False = False
-- True  /^ True = True
-- True  /^ False = True
-- False /^ True = True
--
-- (/^) :: Bool -> Bool -> Bool
-- False /^ b = b
-- True  /^ _ = True
--
-- (/^) :: Bool -> Bool -> Bool
-- a /^ b
--     | a == True = True
--     | b == True = True
--     | otherwise = False

-- Exercise 4
(^) :: Bool -> Bool -> Bool
(^) a b = if a == True && b == True
          then True 
          else False

-- Exercise 5
(^^) :: Bool -> Bool -> Bool
(^^) a b = if a == True 
           then b
           else if a == False
                then False
                else False

-- Exercise 6
mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> (\z -> x * y * z))
