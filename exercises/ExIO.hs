module ExIO where

import Prelude hiding
    ( putStr
    , putStrLn
    , getLine
    , interact
    , (>>)
    , (>>=)
    )
import Text.Read ( readMaybe )

-- read through the whole module first, to get an idea
-- of what's required and to decide where to start

getLine :: IO String
getLine = do x <- getChar
             if x == '\n' then (return []) 
             else 
                 do s <- getLine
                    return (x:s)

--getLine' :: IO String
--getLine' = getChar >> \x -> 

getInt :: IO Int
getInt = do str <- getLine
            return (read str::Int)

getSafeInt :: IO (Maybe Int)
getSafeInt = do str <- getLine
                return (readMaybe str)

-- sequencing: first do f ignoring its result, then do g and keep its result
infixl 1 >>

(>>) :: IO a -> IO b -> IO b
ax >> ay = do ax
              ay

-- pauses till the user presses any normal key
pause :: IO ()
pause = getChar >> skip

skip :: IO ()
skip = do
         return ()

newline :: IO ()
newline = putChar '\n'

-- define it as a foldr
putStr :: String -> IO ()
putStr ""     = skip
putStr (x:xs) = do putChar x
                   putStr xs

-- transform f into one "just like f" except that it prints a newline
-- after any side-effects f may had
lnize :: (a -> IO b) -> a -> IO b
lnize f x = do s <- f x
               newline
               return s
lnize' f x = f x >>= \s -> (newline >> return s)
--lnizar f x = do f x
--                newline

putStrLn :: String -> IO ()
putStrLn = lnize putStr

putCharLn :: Char -> IO ()
putCharLn = lnize putChar

-- reads the entire user input as a single string, transforms it, and prints it
interact :: (String -> String) -> IO ()
interact f = do str <- getLine
                putStrLn (f str)
              
perlineize :: (String -> String) -> (String -> String)
perlineize f = unlines . map f . lines

interactPerLine :: (String -> String) -> IO ()
interactPerLine = interact . perlineize

when :: Bool -> IO () -> IO ()
when p s = if p then s else skip

unless :: Bool -> IO () -> IO ()
unless b = when (not b)

guard :: Bool -> IO ()
guard True  = skip
guard False = error "Did not pass the guard"

forever :: IO a -> IO b
forever s = do s 
               forever s

-- transforms the action given to an equivalent one that has no result
void :: IO a -> IO ()
void a = do skip

-- Kleisli compositions
infixr 1 >=>, <=<

-- diagrammatic order
(>=>) :: (a -> IO b) -> (b -> IO c) -> (a -> IO c)
f >=> g = \x ->
    do r <- f x
       g r

-- traditional order
-- comparison of types:
-- (.)   :: (b ->    c) -> (a ->    b) -> a ->    c
-- (<=<) :: (b -> IO c) -> (a -> IO b) -> a -> IO c
(<=<) :: (b -> IO c) -> (a -> IO b) -> (a -> IO c)
(<=<) = flip (>=>)


-- Bind
infixl 1 >>=

-- bind
(>>=) :: IO a -> (a -> IO b) -> IO b
ax >>= f = do x <- ax
              f x


infixl 4 $>, <$

-- make an action that has the side effects of the action on the left
-- but with result the value on the right
($>) :: IO a -> b -> IO b
iox $> y = undefined

-- vice-versa
(<$) :: a -> IO b -> IO a
x <$ ioy = undefined

ap :: IO (a -> b) -> IO a -> IO b
iof `ap` iox = undefined

filterIO :: (a -> IO Bool) -> [a] -> IO [a]
filterIO f [] = return []
filterIO f (x:xs)  = do
    r <- f x
    if r then filterIO f xs else return []

iomap :: (a -> b) -> IO a -> IO b
iomap = undefined

mapIO :: (a -> IO b) -> [a] -> IO [b]
mapIO f []     = return []
mapIO f (x:xs) = do
    v  <- f x
    vs <- mapIO f xs
    return (v:vs)

zipWithIO :: (a -> b -> IO c) -> [a] -> [b] -> IO [c]
zipWithIO = undefined

zipWithIO_ :: (a -> b -> IO c) -> [a] -> [b] -> IO ()
zipWithIO_ = undefined

sequenceIO :: [IO a] -> IO [a]
sequenceIO = undefined

sequenceIO_ :: [IO a] -> IO ()
sequenceIO_ = undefined

replicateIO :: Integral i => i -> IO a -> IO [a]
replicateIO = undefined

replicateIO_ :: Integral i => i -> IO a -> IO [a]
replicateIO_ = undefined

forIO :: [a] -> (a -> IO b) -> IO [b]
forIO = undefined

forIO_ :: [a] -> (a -> IO b) -> IO ()
forIO_ = undefined

joinIO :: IO (IO a) -> IO a
joinIO = undefined

foldlIO :: (b -> a -> IO b) -> b -> [a] -> IO b
foldlIO = undefined

foldlIO_ :: (b -> a -> IO b) -> b -> [a] -> IO ()
foldlIO_ = undefined
