{-# LANGUAGE DeriveGeneric #-}
module Models where

import Data.Text
import GHC.Generics

data Post = Post
    { id :: Int
    , tipo :: String
    , titulo :: String
    , conteudo :: String
    } deriving (Show, Generic, Eq)
