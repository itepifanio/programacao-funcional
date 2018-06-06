module A27 where

import Prelude hiding ( (>>), (>>=) )
import Text.Printf ( printf )

printTwice :: String -> IO ()
printTwice x = do putStrLn x
                  getLine
                  putStrLn x

printTwice' :: String -> IO ()
printTwice' x = putStrLn x >> getLine >> putStrLn x

greet :: String -> IO ()
greet x  = do putStrLn x
              nome <- getLine
              putStrLn $ x ++ ", " ++  nome
              putStrLn $ concat[x, ", ", nome]
              printf "%s, %s \n" x nome

-- and then
(>>) :: IO a -> IO b -> IO b
ax >> ay = do ax
              ay
-- bind
(>>=) :: IO a -> (a -> IO b) -> IO b
ax >>= f = do x <- ax
              f x

greet' = getLine >>= \x -> printf "hello, %s\n" x 

