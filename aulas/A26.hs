module A26 where

putCharLn :: Char -> IO()
putCharLn c = 
    do putChar c
       newLine

newLine :: IO()
newLine = putChar '\n'

putCharLn' c = putStrLn (c:"")

putStr' :: String -> IO()
putStr' ""      = return ()
putStr' (x:xs)  = do putChar x 
                     putStr' xs
                 
