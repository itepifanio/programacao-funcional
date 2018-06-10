module A28 where

-- batata = do ax
--             ay

-- desugarizando
-- batata' = ax >> ay

-- potato = do x <- ax
--             ay

-- desugarizando
-- potato' = ax >>= \x -> ay

putStr' "" = return ()
putStr' (c:cs) =
    do let charToBePrinted = c
       putChar charToBePrinted
       putStr cs
-- cant do
-- putStr (c:cs) = 
--     do charToBePrinted <- c
--        putChar charToBePrinted
--        putStr cs
--
-- because type of c is char and
-- can't be evaluated

-- No ghci: reads "12 coisas" :: [(Int, String)]

getSafeInt :: IO (Maybe Int)
getSafeInt = 
    do s <- getLine
       let res = reads s :: [(Int, String)]
       case res of 
           [(n, "")] -> return $ Just n
           _         -> return Nothing

{-
import System.IO (hSetEcho, hGetEcho, stdin, stdout )

pause :: IO()
pause = do
    oldEcho <- hGetEcho stdin -- Desativa o echo 
    hGetEcho stdin False
    getChar
    hSetEcho stdin oldEcho
    skip
-}    

forever' :: IO a -> IO b
forever' ax = do ax
                 forever' ax

guard' :: Bool -> IO ()
guard' True  = return () -- skip
guard' False = error "Did not pass the guard"

void :: IO a -> IO ()
void ax = ax >> return () -- skip

-- diagrammatic order
(>=>) :: (a -> IO b) -> (b -> IO c) -> (a -> IO c)
f >=> g = \x ->
    do r <- f x
       g r
       
(<=<) :: (b -> IO c) -> (a -> IO b) -> (a -> IO c)
(<=<) = flip (>=>)

mapIO :: (a -> IO b) -> [a] -> IO [b]
mapIO f []     = return []
mapIO f (x:xs) = do
    v  <- f x
    vs <- mapIO f xs
    return (v:vs)

