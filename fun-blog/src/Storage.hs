{-# LANGUAGE OverloadedStrings #-}
module Storage where

import qualified Models as M
import qualified Data.Text as Txt
import Control.Monad
import Data.Maybe

import Database.SQLite.Simple as Sql
import Database.SQLite.Simple.Types as SqlTypes

instance Sql.FromRow M.Post where
    fromRow = M.Post <$> Sql.field <*> Sql.field  <*> Sql.field  <*> Sql.field

selectPosts :: Sql.Connection -> IO [M.Post]
selectPosts conn =
    Sql.query_ conn "select * from post" :: IO [M.Post]
