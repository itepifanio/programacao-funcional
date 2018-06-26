{-# LANGUAGE OverloadedStrings #-}
module Main where
import Web.Scotty
import qualified Data.Text.Lazy as T
import qualified Text.Blaze.Html5 as H

data Post = Post 
    { tipo :: T.Text
    , titulo :: T.Text
    , conteudo :: H.Html 
    }

instance Show Post where
    show post = 
        T.unpack $ titulo post

toPost :: T.Text -> Maybe Post


main :: IO ()
main = do
    putStrLn "Starting Server..."
    scotty 3000 $ do
        get "/" $ do
            text "hello world!"
        get "/home" $ file "templates/index.html"
            
