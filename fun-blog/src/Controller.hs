{-# LANGUAGE OverloadedStrings #-}

module Controller where

import Prelude hiding (id)
import Control.Monad.IO.Class  (liftIO)
import Web.Scotty
import qualified Data.Text.Lazy as T
import Models as M
import Database.SQLite.Simple as Sql
import System.IO.Unsafe (unsafePerformIO)

type id = Int
type tipo = T.Text
type titulo = T.Text
type conteudo = T.Text

-- Realiza consulta sql e traz os dados
allPosts :: Sql.Connection -> IO [M.Post]
allPosts conn = do
    Sql.query_ conn "select id, tipo, titulo, conteudo from posts" :: IO [M.Post]

-- Transforma uma lista de posts em json
jsonPosts :: [M.Post] -> ActionM ()
jsonPosts posts = json posts

lastPost :: Sql.Connection -> [M.Post]
lastPost  conn = unsafePerformIO $ do
    Sql.query_ conn "select max(id), titulo, conteudo, tipo from posts" :: IO [M.Post]

-- Recupera o último id do último post
lastId :: Sql.Connection -> Int
lastId conn = (id (lastPost conn !! 0))

-- Recebe o titulo, conteudo e tipo de post e cria um post
modelPost :: Sql.Connection -> tipo -> titulo -> conteudo -> M.Post
modelPost conn tp t c =
    M.Post {
    id = (lastId conn) + 1,
    tipo     = tp,
    titulo   = t,
    conteudo = c
    }

-- Insere post no banco de dados
insertPost :: ToRow b => b -> IO b
insertPost post = do
    conn <- Sql.open "db.sqlite3"
    Sql.execute conn "insert into posts (id, tipo, titulo, conteudo) values (?,?,?,?)" (post)
    return post

-- Deleta um post do banco de dados
deletePost :: Sql.Connection -> id -> IO ()
deletePost conn id = Sql.execute conn "delete from posts where id = ?" (Only id)

-- Encontra post através do id
findPost :: Sql.Connection -> id -> IO [M.Post]
findPost conn id = do
    p <- Sql.queryNamed conn "select * from posts where id = :id" [":id" := id] :: IO [M.Post]
    return p

-- Recebe os parámetros e realiza a edição dos seus dados
updatePost :: Sql.Connection -> id -> titulo -> conteudo -> IO ()
updatePost conn id titulo conteudo =
    Sql.executeNamed conn
        "update posts set titulo = :titulo, conteudo = :conteudo WHERE id = :id" [":titulo" := titulo, ":conteudo" := conteudo, ":id" := id]


{- mkPost :: Sql.Connection -> T.Text -> T.Text -> T.Text -> IO (Maybe M.Post)
mkPost conn titulo conteudo tipo
    | T.null titulo   == False = return Nothing
    | T.null conteudo == False = return Nothing
    | T.null tipo     == False = return Nothing
    | otherwise = return $ Just (modelPost conn titulo conteudo tipo)
-}

mkPost :: Sql.Connection -> tipo -> titulo -> conteudo -> IO (Either T.Text M.Post)
mkPost conn titulo conteudo tipo
    | T.null titulo   == True = return $ Left (T.pack "Nenhum titulo adicionado ao post")
    | T.null conteudo == True = return $ Left (T.pack "Nenhum conteudo adicionado ao post")
    | T.null tipo     == True = return $ Left (T.pack "Nenhum tipo foi adicionado ao post")
    | otherwise = return $ Right $ modelPost conn titulo conteudo tipo
