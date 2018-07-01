{-# LANGUAGE OverloadedStrings #-}
module Main where
import Web.Scotty
import Control.Monad.IO.Class  (liftIO)
import qualified Bootstrap as B
import qualified Storage as S
import Models as M
import qualified Data.Text as T
import Control.Monad (forM_)
import Database.SQLite.Simple as Sql
import Control.Exception (bracket)
import Data.Text (Text)
import Control.Applicative
import Data.Aeson hiding (json)
import Servant

testConnect :: IO Sql.Connection
testConnect = Sql.open "db.sqlite3"

withTestConnection :: (Sql.Connection -> IO a) -> IO a
withTestConnection cb =
    withConn $ \conn -> cb conn
    where
        withConn = bracket testConnect Sql.close
{-
main :: IO ()
main = do
    withTestConnection $ \conn -> do
        B.bootstrapDB conn
        scotty 3000 $ do
            get "/" $ do
                        text "OK! :D"
-}

instance ToJSON M.Post
instance FromJSON M.Post

postsToString :: [M.Post] -> String -> String
postsToString [] _      = ""
postsToString (x:xs) e  = show x ++ e

{-
postsToJson :: IO [M.Post] -> String
postsToJson ioposts = do
    posts <- ioposts
    if posts == "" then show posts else postsToString posts [")"]
-}

allPosts = withTestConnection $ \conn -> do
    Sql.query_ conn "select id,titulo,conteudo,tipo from posts" :: IO [M.Post]


listPosts :: [M.Post] -> ActionM ()
listPosts posts = json posts



{- do
     :: IO [M.Post] -}

main :: IO ()
main = do
    conn <- Sql.open "db.sqlite3"
    --    B.createSchema conn
    --    B.populateData conn
    putStrLn "Starting Server..."
    scotty 3000 $ do
        get "/" $ file "templates/index.html"
        get "/posts" $ do
            posts <- liftIO $ allPosts
            listPosts posts

{-
main :: IO ()
main = do
    putStrLn "Starting Server..."
    scotty 3000 $ do
        get "/" $ file "templates/index.html"
        get "/posts" $ do
             posts' <- newMVar posts <- withTestConnection $ \conn -> do
                postsToJson Sql.query_ conn "select * from post" :: IO [M.Post]
             posts <- liftIO $ readMVar posts'
             json posts
-}

--            json (["Hello, world!", "Oi"] :: [String])
