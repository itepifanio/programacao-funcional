module ExMaybe where

-- Do not alter this import!
import Prelude hiding ( maybe, Maybe(..) )
import qualified Data.Maybe as M

data Maybe a = Nothing | Just a
    deriving (Show, Eq, Ord)

catMaybes :: [Maybe a] -> [a]
catMaybes xs = [x | Just x <- xs]

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing  = error "Nothing can't be resolved"

fromMaybe :: a -> Maybe a -> a
fromMaybe x y = case y of
    Nothing -> x
    Just v  -> v

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _       = True

isNothing :: Maybe a -> Bool
isNothing x = not $ isJust x

listToMaybe :: [a] -> Maybe a
listToMaybe []     = Nothing
listToMaybe (x:xs) = Just x

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f []     = []
mapMaybe f (x:xs) = 
    let rs = mapMaybe f xs in
    case f x of
         Nothing -> rs
         Just r  -> r:rs

maybe :: b -> (a -> b) -> Maybe a -> b
maybe x _ Nothing = x
maybe x f y       = f (fromJust y)

maybeToList :: Maybe a -> [a]
maybeToList Nothing   = []
maybeToList (Just x)  = [x]


