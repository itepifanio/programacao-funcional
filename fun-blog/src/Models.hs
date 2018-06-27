{-# LANGUAGE DeriveGeneric #-}
module Models where

import GHC.Generics

data Post = Post
    { id :: Int
    , tipo :: String
    , titulo :: String
    , conteudo :: String
    } deriving (Show, Generic)
