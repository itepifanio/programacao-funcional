module ExMatrix2x2
    ( matrix
    , zero
    , identity
    , rows
    , cols
    , getElem
    , transpose
    , det
    , isDiagonal
    , isTriangular
    , isLowerTriangular
    , isUpperTriangular
    -- Inconsistência sem o is
    -- Haskell não utiliza o padrao is
    , singular
    , invertible
    , inverse
    ) where

-- Pode-se criar coisas mais gerais
-- tipo numero ser Num a => a
type Number = Double
type Row = [Number]
type Col = [Number]

--data Matrix2x2 = Matrix2x2 Number Number Number Number

--instance Show Matrix2x2 where
--    show (Matrix2x2 a b c d) = "[" ++ "[" ++ (show a) ++ "," ++ (show b) ++ "]" ++ "[" ++ (show c) ++ "," ++ (show d) ++ "]" ++ "]"
   
data Matrix2x2 = Matrix2x2 ((Number, Number), (Number, Number))

-- Trabalhando com matriz no formato
-- [[ a , b ],
--  [ c , d ]]
instance Show Matrix2x2 where
    show (Matrix2x2 ((a,b),(c,d))) = "[" ++ "[" ++ (show a) ++ "," ++ (show b) ++ "]" ++ "[" ++ (show c) ++ "," ++ (show d) ++ "]" ++ "]"


instance Eq Matrix2x2 where
    (==) (Matrix2x2 ((a,b),(c,d))) (Matrix2x2 ((a',b'),(c',d'))) = a == a' && b == b' && c == c' && d == d'

-- Define multiplicacao de uma linha por uma coluna
--(***) :: Row -> Col -> Number
--(***) (Row [a,b]) (Col [c,d]) = a * c + b * d

instance Num Matrix2x2 where
    (+) (Matrix2x2 ((a,b),(c,d))) (Matrix2x2 ((a',b'),(c',d'))) = (Matrix2x2  ((a+a', b+b'), (c+c', d+d')))
    (*) (Matrix2x2 ((a,b),(c,d))) (Matrix2x2 ((a',b'),(c',d'))) = (Matrix2x2 ((a*a' + b*c',a*b' + b*d'), (c*a' + d*c', c*b' + d*d')))

    negate = undefined
    abs = undefined
    signum = undefined
    fromInteger = undefined

-- matrix a b c d should create the matrix
-- ( a c )
-- ( b d )
matrix :: Number -> Number -> Number -> Number -> Matrix2x2
matrix = undefined

zero :: Matrix2x2
zero = undefined

identity :: Matrix2x2
identity = undefined

rows :: Matrix2x2 -> [Row]
rows = undefined

cols :: Matrix2x2 -> [Col]
cols = undefined

getElem :: (Int,Int) -> Matrix2x2 -> Number
getElem = undefined

transpose :: Matrix2x2 -> Matrix2x2
transpose = undefined

det :: Matrix2x2 -> Number
det = undefined

isDiagonal :: Matrix2x2 -> Bool
isDiagonal = undefined

isTriangular :: Matrix2x2 -> Bool
isTriangular = undefined

isLowerTriangular :: Matrix2x2 -> Bool
isLowerTriangular = undefined

isUpperTriangular :: Matrix2x2 -> Bool
isUpperTriangular = undefined

singular :: Matrix2x2 -> Bool
singular = undefined

invertible :: Matrix2x2 -> Bool
invertible = not . singular

inverse :: Matrix2x2 -> Matrix2x2
inverse = undefined


