module A30 where

import System.IO
import System.IO.Unsafe

greet :: String -> String
greet x = x ++ "," ++ name
    where name = runIO getLine

runIO = unsafePerformIO

{-
Não podemos fazer:

cheat :: IO a -> a
cheat ax = 
    do x <- ax
       x
-}

-- nonDeterministicPlus :: [Int] -> [Int] -> [Int] 
ndPlus :: [Int] -> [Int] -> [Int]
-- ndPlus xs ys = [x + y | x <- xs, y <- ys]
ndPlus xs ys = do
    x <- xs
    y <- ys
    return $ x + y

{-
return :: a -> [a]
(>>=) :: [a] -> (a -> [b]) -> [b]
-}
