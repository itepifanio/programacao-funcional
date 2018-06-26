{-# LANGUAGE OverloadedStrings #-}
module Main where
import Web.Scotty

main :: IO ()
main = do
    putStrLn "Starting Server..."
    scotty 3000 $ do
        get "/" $ do
            text "hello world!"
