{-# LANGUAGE PostfixOperators #-}

module A09Ext where

fact :: Integer -> Integer
fact n = product [1..n]
(!) = fact

-- Sinonimos de tipos {{{
--type String = [Char]
--type Word = String
--type Phrase = String

--break :: Phrase -> [Word]
-- }}}

-- Criando (Integer, Bool)
data PairIntBool = MkPairIntBool Integer Bool
-- bottom :: PaitIntBool
-- bottom = bottom

data Person = Amigo String
            | Colega String String
     deriving (Show, Eq)

data Pair a b = Pair a b    
    deriving (Show, Eq)
