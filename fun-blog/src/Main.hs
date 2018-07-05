{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Control.Monad.IO.Class  (liftIO)
-- import qualified Bootstrap as B
import Controller as C
import Database.SQLite.Simple as Sql
import Data.Text.Lazy
import Network.Wai.Middleware.Static
-- import Data.Maybe (isNothing)
import Data.Either



main :: IO ()
main = do
    conn <- Sql.open "db.sqlite3"
    -- B.createSchema conn
    -- B.populateData conn
    putStrLn "Starting Server..."
    scotty 3000 $ do
        middleware $ staticPolicy (noDots >-> addBase "templates")

        get "/" $ file "templates/index.html"

        get "/posts" $ do
            posts <- liftIO $ C.allPosts conn
            C.jsonPosts posts

        get "/create" $ file "templates/create.html"

        post "/store" $ do
            titulo   <- param "titulo" :: ActionM Text
            conteudo <- param "conteudo" :: ActionM Text
            tipo     <- param "tipo" :: ActionM Text
            handle <- liftIO $ C.mkPost conn titulo conteudo tipo
            if isLeft handle == True
            then do
                html $ fromLeft "hue" handle
            else do
                _ <- liftIO $ C.insertPost $ C.modelPost conn tipo titulo conteudo
                redirect "/"

        get "/destroy/:id" $ do
            id' <- param "id" :: ActionM Int
            _ <- liftIO $ C.deletePost conn id'
            redirect "/"

        get "/jsonEdit/:id" $ do
            id' <- param "id" :: ActionM Int
            q <- liftIO $ C.findPost conn id'
            if q == []
            then do html $ "Nenhuma postagem encontrada"
            else do json q

        get "/edit/:id" $ file "templates/edit.html"

        post "/update/:id" $ do
            id'      <- param "id" :: ActionM Int
            titulo   <- param "titulo" :: ActionM Text
            conteudo <- param "conteudo" :: ActionM Text
            _ <- liftIO $ C.updatePost conn id' titulo conteudo
            redirect "/"
