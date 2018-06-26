{-# LANGUAGE OverloadedStrings #-}
module Main where
import Web.Scotty
import qualified Bootstrap as B
import Database.SQLite.Simple as Sql
import Control.Exception (bracket)
import Data.Text (Text)

testConnect :: IO Sql.Connection
testConnect = Sql.open "db.sqlite3"

withTestConnection :: (Sql.Connection -> IO a) -> IO a
withTestConnection cb =
    withConn $ \conn -> cb conn
    where
        withConn = bracket testConnect Sql.close

main :: IO ()
main = do
    withTestConnection $ \conn -> do
        B.bootstrapDB conn
        scotty 3000 $ do
            get "/" $ do
                        text "OK! :D"

{-
main :: IO ()
main = do
    putStrLn "Starting Server..."
    scotty 3000 $ do
        get "/" $ do
            text "hello world!"
        get "/home" $ file "templates/index.html"
-}            
