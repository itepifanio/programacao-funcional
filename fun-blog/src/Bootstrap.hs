{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Bootstrap where

import Database.SQLite.Simple as Sql
import Database.SQLite.Simple.Types as SqlTypes
import qualified Text.Blaze.Html5 as H
import qualified Data.Text.Lazy as T

bootstrapDB :: Sql.Connection -> IO ()
bootstrapDB conn = do
    createSchema conn
    populateData conn


createSchema :: Sql.Connection -> IO ()
createSchema conn = do
    executeDB "PRAGMA foreign_keys = ON"
    executeDB "create table posts (id integer primary key asc, tipo varchar2(255), conteudo varchar(255), titulo varchar(200))"
    where executeDB = Sql.execute_ conn

posts :: [(Int, String, String, String)]
posts = [(1, "homework", "Criar um blog pro meu website", "<h4>Que consiga suportar html</h4>"),
         (2, "prerequisitos", "Quase nenhum", "<i>Seria bom fmc</i>"),
         (3, "bibliografia", "Meu livro", "<strong>Depois me digam o que acharam</strong>")]

populateData :: Sql.Connection -> IO ()
populateData conn = do
    mapM_ insertPosts posts
    where
        insertPosts ps = Sql.execute conn "insert into posts (id, tipo, conteudo, titulo) values (?,?,?,?)" ps
