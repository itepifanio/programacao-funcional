{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Control.Monad.IO.Class  (liftIO)
import qualified Bootstrap as B
import Controller as C
import Database.SQLite.Simple as Sql
-- import Data.Text (Text)
import Data.Aeson hiding (json)
import Data.Monoid (mconcat)
import Data.Either
import Data.Text

main :: IO ()
main = do
    conn <- Sql.open "db.sqlite3"
    --    B.createSchema conn
    --    B.populateData conn
    putStrLn "Starting Server..."
    scotty 3000 $ do
        get "/" $ file "templates/index.html"
        get "/posts" $ do
            posts <- liftIO $ C.allPosts conn
            C.jsonPosts posts
        get "/create" $ file "templates/create.html"
        post "/store" $ do
            titulo   <- param "titulo"
            conteudo <- param "conteudo"
            tipo     <- param "tipo"
            let mkPost = C.mkPost titulo conteudo tipo
            if isLeft mkPost == True
            then do
                html $ fromLeft (pack "error") (fromStrict mkPost)
            else do
                fromRight (pack "error") mkPost
                redirect "/"
