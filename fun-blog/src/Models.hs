{-# LANGUAGE DeriveGeneric #-}
module Models where

import GHC.Generics
import qualified Data.Text.Lazy as T
import qualified Text.Blaze.Html5 as H

data Post = Post 
    { id :: Int
    ,  tipo :: T.Text
    , titulo :: T.Text
    , conteudo :: H.Html 
    }

instance Show Post where
    show post = 
        T.unpack $ titulo post

