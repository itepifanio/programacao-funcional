module Hangman where

import Data.Char ( toLower, toUpper )
import Data.List (intersperse)
import Data.Maybe (isJust)
import System.Exit (exitSuccess)
import Control.Monad (forever)
import System.IO
    ( hSetEcho
    , hGetEcho
    , stdin
    , stdout
    )


data Hangman = Hangman {
    palavra  :: String,
    palavra' :: [Maybe Char],
    tentativas :: String
} deriving (Show, Eq, Ord)

hangman palavra = Hangman {
    palavra = palavra,
    palavra' = replicate (length palavra) Nothing,
    tentativas = ""
}

toChar :: Maybe Char -> Char
toChar Nothing  = '-'
toChar (Just x) = x

isCharPalavra :: Hangman -> Char -> Bool
isCharPalavra hangman s = s `elem` palavra hangman        

tentativa :: Hangman -> Char -> Hangman
tentativa h c =
        Hangman {
            palavra = palavra h,
            tentativas = c : tentativas h,
            palavra' = xablau (palavra h) c [] (palavra' h)         
        }

echoless :: IO a -> IO a
echoless ax =
    do oldEcho <- hGetEcho stdin
       hSetEcho stdin False
       x <- ax
       hSetEcho stdin oldEcho
       return x

-- Por conta da tipagem, trabalha com o hangman
-- retornando IO Hangman, pensar em melhor solução
-- na hora de imprimir hangman durante o loop
handleTry :: Hangman -> Char -> IO Hangman
handleTry hangman c = do
    printHagman (palavra' hangman)
    return $ tentativa hangman c


-- Função cheia das gambiarra que retorna a lista 
-- de palavra' com os just a partir do que foi pesquisado
-- palavra -> c -> [] -> palavra'
-- No ghci: xablau "abc" 'b' [] [Nothing, Nothing, Nothing]      
--          xablau "abc" 'c' [] it
xablau :: String -> Char -> [Maybe Char] -> [Maybe Char] -> [Maybe Char]
xablau [] c ms [] = ms
xablau (x:xs) c ms (p:ps')
    | c == x && p == Nothing = Just c : xablau xs c ms ps'
    | c /= x && p == Nothing = Nothing : xablau xs c ms ps'
    | otherwise = p : xablau xs c ms ps'

printHagman :: [Maybe Char] -> IO ()
printHagman [] = return ()
printHagman (x:xs) = 
    do putChar (toChar x)
       printHagman xs


endGame :: Hangman -> IO ()
endGame hangman =
  if all isJust (palavra' hangman) then
    do
      putStrLn $ "\nVocê acertou a palavra era " ++ map toUpper (palavra hangman) ++ ", parabéns"
      exitSuccess
  else
    return ()

gameLost :: Hangman -> IO ()
gameLost hangman =
  if length (tentativas hangman) > 5 then
    do
      putStrLn $ "\nPerdeu"
      exitSuccess
  else
    return () 

runGame :: Hangman -> IO ()
runGame hangman = forever $ do
    -- forever tem que receber um IO () IO(), então
    -- criei essas duas funções
    endGame hangman
    gameLost hangman
    putStr "\n"
    putStr "Tente uma letra: \n"
    c <- echoless getChar
    handleTry hangman c >>= runGame
    handleTry hangman c

main :: IO ()
main = do
    runGame (hangman "batata")
    


