{-# LANGUAGE OverloadedStrings #-}
module Controller where

import Web.Scotty
import Models as M
import Data.Aeson hiding (json)
import Database.SQLite.Simple as Sql

instance ToJSON M.Post
instance FromJSON M.Post

instance Sql.FromRow M.Post where
    fromRow = M.Post <$> Sql.field <*> Sql.field  <*> Sql.field  <*> Sql.field

allPosts conn = do
    Sql.query_ conn "select id,titulo,conteudo,tipo from posts" :: IO [M.Post]


listPosts :: [M.Post] -> ActionM ()
listPosts posts = json posts
