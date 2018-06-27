{-# LANGUAGE OverloadedStrings #-}
module Main where
import Web.Scotty
import Control.Monad.IO.Class  (liftIO)
import qualified Bootstrap as B
import qualified Storage as S
import Models as M
import Database.SQLite.Simple as Sql
import Control.Exception (bracket)
import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON)

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

main :: IO ()
main = do
    putStrLn "Starting Server..."
    scotty 3000 $ do
        get "/" $ file "templates/index.html"
        get "/posts" $ do
            json posts where
            posts = withTestConnection $ \conn -> do
                S.selectPosts conn
