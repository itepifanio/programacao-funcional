{-# LANGUAGE DeriveGeneric #-}
module Models where

import qualified Data.Text as T
import GHC.Generics

data Post = Post
    { tipo :: T.Text
    , titulo :: T.Text
    , conteudo :: T.Text
    } deriving (Show, Generic, Eq)
