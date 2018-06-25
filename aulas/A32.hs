module TND where
import Control.Applicative 

{-
type Cryptotext = String
type Normaltext = String
 -
 -
 - Má ideia, pois é apenas um string para string
 - e portanto permite encriptar um texto já encriptado
 - por exemplo
encrypt :: Normaltext -> Cryptotext

decrypt :: Cryptotext -> Normaltext
-}

data Cryptotext = Cryptotext String
    deriving (Show, Eq)

data Normaltext = Normaltext String
    deriving (Show, Eq)

displayNormal :: Normaltext -> String
displayNormal (Normaltext x) = x

newtype Ctext = Ctext String
    deriving (Show, Eq)
newtype Ntext = Ntext String
    deriving (Show, Eq)

bottom :: a
bottom = bottom

oi :: Ctext
oi = Ctext bottom

io :: Ctext
io = bottom

--import Data.Semigroup as S

newtype Additive = Additive Int

--newtype Multiplicative = Multiplicative Int

--fromAdditive (Additive x) = x

{- instance Semigroup Additive where
   Additive x (<>) Additive y = Additive (x + y)
-}   

{-
module List where
-}
type Lista = []

-- Lista não determinismo
instance Applicative Lista where
    pure :: a -> Lista a
    pure x = [x]

    (<*>) :: Lista (a -> b) -> Lista a -> Lista b
    fs <*> xs = [f x | f <- fs, x <- xs]

