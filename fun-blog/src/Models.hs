{-# LANGUAGE DeriveGeneric #-}
module Models where

import qualified Data.Text.Lazy as T
import Database.SQLite.Simple as Sql
import Data.Aeson hiding (json)
import GHC.Generics

data Post = Post
    { id :: Int
    , tipo :: T.Text
    , titulo :: T.Text
    , conteudo :: T.Text
    } deriving (Show, Generic, Eq)

instance ToJSON Post
instance FromJSON Post

instance Sql.FromRow Post where
    fromRow = Post <$> Sql.field  <*> Sql.field  <*> Sql.field  <*> Sql.field

instance ToRow Post where
    toRow (Post id tipo conteudo titulo) = toRow (id, tipo, conteudo, titulo)
