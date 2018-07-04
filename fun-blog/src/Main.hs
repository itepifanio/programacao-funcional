{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Control.Monad.IO.Class  (liftIO)
import qualified Bootstrap as B
import Controller as C
import Database.SQLite.Simple as Sql
import Data.Aeson hiding (json)
import Data.Text.Lazy
import System.IO.Unsafe (unsafePerformIO)
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
            handle <- liftIO $ C.mkPost titulo conteudo tipo
            if isLeft handle == True
            then do
                html $ fromLeft "hue" handle
            else do
                liftIO $ C.insertPost $ C.modelPost titulo conteudo tipo
                redirect "/"
