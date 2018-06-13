module IOkit where

import Prelude hiding
    ( putStr
    , putStrLn
    , getLine
    , interact
    , (>>)
    , (>>=)
    )

import System.IO
    ( hSetEcho
    , hGetEcho
    , stdin
    , stdout
    )

getLine :: IO String
getLine = do
    c <- getChar
    if isEOL c
       then return ""
       else do l <- getLine
               return (c:l)

isEOL :: Char -> Bool
isEOL = (== '\n')

getInt :: IO Int
getInt = iomap read getLine

-- first definition:
-- getInt =
--     do s <- getLine
--        return $ read s

-- also valid one-liner:
-- getInt = getLine >>= (return . read)

getSafeInt :: IO (Maybe Int)
getSafeInt =
  do s <- getLine
     let parsed = reads s :: [(Int, String)]
     case parsed of
       [(n, "")] -> return $ Just n
       _         -> return Nothing

-- sequencing: first do f ignoring its result, then do g and keep its result
infixl 1 >>

(>>) :: IO a -> IO b -> IO b
ax >> ay = do ax
              ay

-- transforms the action given to an equivalent one that has no result
void :: IO a -> IO ()
void ax = ax >> skip

-- pauses till the user presses any normal key
pause :: IO ()
pause = void $ echoless getChar

-- transform an action ax to one that makes sure stdin echo is off
echoless :: IO a -> IO a
echoless ax =
    do oldEcho <- hGetEcho stdin
       hSetEcho stdin False
       x <- ax
       hSetEcho stdin oldEcho
       return x

skip :: IO ()
skip = return ()

newline :: IO ()
newline = putChar '\n'

-- define it as a foldr
putStr :: String -> IO ()
putStr ""     = skip
putStr (c:cs) =
    do putChar c
       putStr cs

-- transform f into one "just like f" except that it prints a newline
-- after any side-effects f may had
lnize :: (a -> IO b) -> a -> IO b
lnize f x = do s <- f x
               newline
               return s

-- diet version:
-- lnize' f x = f x >>= \s ->
--                (newline >> return s)

putStrLn :: String -> IO ()
putStrLn = lnize putStr

putCharLn :: Char -> IO ()
putCharLn = lnize putChar

-- reads the entire user input as a single string, transforms it, and prints it
interact :: (String -> String) -> IO ()
interact = undefined

perlineize :: (String -> String) -> (String -> String)
perlineize f = unlines . map f . lines

interactPerLine :: (String -> String) -> IO ()
interactPerLine = interact . perlineize

when :: Bool -> IO () -> IO ()
when b ax = do if b then ax else skip

unless :: Bool -> IO () -> IO ()
unless = when . not

guard :: Bool -> IO ()
guard True  = skip
guard False = error "guard: fail"

forever :: IO a -> IO b
forever ax = do ax
                forever ax

-- Kleisli compositions
infixr 1 >=>, <=<

-- diagrammatic order
(>=>) :: (a -> IO b) -> (b -> IO c) -> (a -> IO c)
f >=> g =
    \x -> do r <- f x
             g r


-- traditional order
-- comparison of types:
-- (.)   :: (b ->    c) -> (a ->    b) -> (a ->    c)
-- (<=<) :: (b -> IO c) -> (a -> IO b) -> (a -> IO c)
(<=<) :: (b -> IO c) -> (a -> IO b) -> (a -> IO c)
(<=<) = flip (>=>)


-- Bind
infixl 1 >>=

(>>=) :: IO a -> (a -> IO b) -> IO b
ax >>= f = do x <- ax
              f x


infixl 4 $>, <$

-- make an action that has the side effects of the action on the left
-- but with result the value on the right
($>) :: IO a -> b -> IO b
ax $> y = ax >> return y

-- vice-versa
(<$) :: a -> IO b -> IO a
(<$) = flip ($>)

-- ap-ply
ap :: IO (a -> b) -> IO a -> IO b
af `ap` ax = do
    f <- af
    x <- ax
    return $ f x

filterIO :: (a -> IO Bool) -> [a] -> IO [a]
filterIO p []     = return []
filterIO p (x:xs) = do
    b <- p x
    xs' <- filterIO p xs
    return $ if b then (x:xs') else xs'

sequenceIO :: [IO a] -> IO [a]
sequenceIO []       = return []
sequenceIO (ax:axs) = do
    x  <- ax
    xs <- sequenceIO axs
    return $ x : xs

sequenceIO_ :: [IO a] -> IO ()
sequenceIO_ = void . sequenceIO

iomap :: (a -> b) -> IO a -> IO b
--iomap f ax = do x <- ax
--                return $ f x
iomap f ax = ax >>= (return . f)

mapIO :: (a -> IO b) -> [a] -> IO [b]
mapIO f = sequenceIO . map f

mapIO_ :: (a -> IO b) -> [a] -> IO ()
mapIO_ f = sequenceIO_ . map f
-- mapIO_ f = void . mapIO

-- primitive solution
-- mapIO f []     = return []
-- mapIO f (x:xs) = do
--    v  <- f x
--    vs <- mapIO f xs
--    return (v:vs)

----------------------------------------------------------------

zipWithIO :: (a -> b -> IO c) -> [a] -> [b] -> IO [c]
zipWithIO = undefined

zipWithIO_ :: (a -> b -> IO c) -> [a] -> [b] -> IO ()
zipWithIO_ = undefined

replicateIO :: Integral i => i -> IO a -> IO [a]
replicateIO = undefined

replicateIO_ :: Integral i => i -> IO a -> IO ()
replicateIO_ = undefined

forIO :: [a] -> (a -> IO b) -> IO [b]
forIO = flip mapIO

{-
forIO_ :: [a] -> (a -> IO b) -> IO ()
forIO_ = void forIO
-}

joinIO :: IO (IO a) -> IO a
joinIO aax = do
    ax <- aax
    ax

foldlIO :: (b -> a -> IO b) -> b -> [a] -> IO b
foldlIO = undefined

foldlIO_ :: (b -> a -> IO b) -> b -> [a] -> IO ()
foldlIO_ = undefined



