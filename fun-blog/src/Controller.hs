{-# LANGUAGE OverloadedStrings #-}
module Controller where

import Prelude hiding (id)
import Web.Scotty
import Models as M
import Data.Aeson hiding (json)
import Database.SQLite.Simple as Sql
import Control.Monad.IO.Class (liftIO)
import System.IO.Unsafe (unsafePerformIO)

instance ToJSON M.Post
instance FromJSON M.Post

instance Sql.FromRow M.Post where
    fromRow = M.Post <$> Sql.field <*> Sql.field  <*> Sql.field  <*> Sql.field

instance ToRow M.Post where
    toRow (M.Post id tipo titulo conteudo) = toRow (id, tipo, titulo, conteudo)

-- Realiza consulta sql e traz os dados
allPosts :: Sql.Connection -> IO [M.Post]
allPosts conn = do
    Sql.query_ conn "select id,titulo,conteudo,tipo from posts" :: IO [M.Post]

-- Transforma uma lista de posts em json
jsonPosts :: [M.Post] -> ActionM ()
jsonPosts posts = json posts

-- Recupera o último post contido no banco
lastPost :: [M.Post]
lastPost = unsafePerformIO $ do
    conn <- Sql.open "db.sqlite3"
    Sql.query_ conn "select max(id), titulo, conteudo, tipo from posts" :: IO [M.Post]

-- Recupera o último id do último post
lastId :: Int
lastId = (id (lastPost !! 0))

-- Recebe o titulo, conteudo e tipo de post e cria um post
modelPost :: String -> String -> String -> M.Post
modelPost t c tp =
    M.Post {
    id       = lastId + 1,
    titulo   = t,
    conteudo = c,
    tipo     = tp
    }

-- Insere post no banco de dados
insertPost :: ToRow b => b -> IO b
insertPost post = do
    conn <- Sql.open "db.sqlite3"
    Sql.execute conn "insert into posts (id, tipo, titulo, conteudo) values (?,?,?,?)" (post)
    return post

mkPost :: [String] -> Either String M.Post
mkPost xs
    | xs == [[]] = Left "Nenhum dado foi adicionado ao post"
    | xs !! 0 == [] = Left "Nenhum titulo adicionado ao post"
    | xs !! 1 == [] = Left "Nenhum conteudo adicionado ao post"
    | xs !! 2 == [] = Left "Nenhum tipo foi adicionado ao post"
    | otherwise = Right $ modelPost (xs !! 0 !! 1) (xs !! 0 !! 1) (xs !! 0 !! 1)
