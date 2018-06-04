module A26 where

putCharLn :: Char -> IO()
putCharLn c = 
    do putChar c
       newLine

lnizar :: (a -> IO()) -> a -> IO()
lnizar f x = do f x
                newline

putCharLn'' = lnizar putChar
putStrLn'' = lnizar putStr

newLine :: IO()
newLine = putChar '\n'

 
--putCharLn' c = putStrLn (c:"")

--putStr' :: String -> IO()
--putStr' ""      = return ()
--putStr' (x:xs)  = do putChar x 
--                     putStr' xs

--putStrLn' :: String -> IO()
--putStrLn' x = do putStr' x
--                 newLine 

conversa :: IO()
conversa = do
    putStrLn "hello, whats your name?"
    name <- getLine
    putStrLn $ "Nice to meet you" ++ name
