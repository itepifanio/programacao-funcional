module ExEither where

-- Do not alter this import!
import Prelude hiding ( either, Either(..) )
import qualified Data.Either as E

data Either a b = Left a | Right b
    deriving (Show, Eq)

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left y)  = f y
either _ g (Right x) = g x

fromLeft :: a -> Either a b -> a
fromLeft _ (Left y) = y
fromLeft x _        = x

fromRight :: b -> Either a b -> b
fromRight _ (Right y) = y
fromRight x _         = x

isLeft :: Either a b -> Bool
isLeft (Left y) = True
isLeft _        = False

isRight :: Either a b -> Bool
isRight x = not $ isLeft x

lefts :: [Either a b] -> [a]
lefts xs = [x | Left x <- xs]


partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers [] = ([], [])
partitionEithers xs = (left, right)
    where right = rights xs
          left  = lefts  xs

rights :: [Either a b] -> [b]
rights xs = [x | Right x <- xs ]

