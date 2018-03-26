module ExRat
    ( rat
    , (//)
    , denominator
    , numerator
    ) where

data Rat = Rat Integer Integer

instance Show Rat where
    show (Rat a b) = (show a) ++ "/" ++ (show b)

-- Still got error on equality    
-- Change `quot` to avoid division by zero
quot' :: Integer -> Integer -> Integer
quot' a b = if a >= b 
           then a `quot` b
           else b `quot` a

instance Eq Rat where  
    (==) (Rat (a) b) (Rat a' b') =
        if (quot' a b) `mod` (quot' a' b') == 0 
        then True
        else False

instance Num Rat where
    (+) (Rat a b) (Rat a' b') = Rat n m
        where n = b' * a + a' * b
              m = b' * b  
    (*) (Rat a b) (Rat a' b') = Rat n m
        where n = a * a'
              m = b * b' 
        
    negate (Rat a b) = Rat n m
        where n = a * (-1)
              m = b
    
    abs (Rat a b)
                 | n < 0 = (Rat (n*(-1)) m)
                 | m < 0 = (Rat n (m*(-1))) 
                 | otherwise = Rat a b
                 where 
                    n = a 
                    m = b              
            
    signum (Rat a b) =
        if a == 0 then 0
        else if b > 0 then
             if a < 0 then (-1)
             else 1
        else if a < 0 then 1
        else (-1)
        
    fromInteger i = Rat i 1

instance Ord Rat where
    compare = undefined

rat :: Integer -> Integer -> Rat
rat = undefined

(//) :: Rat -> Rat -> Rat
(//) (Rat a b) (Rat a' b') = Rat n m
    where n = a * b'
          m = b * a'

denominator :: Rat -> Integer
denominator (Rat _ b) = b

numerator :: Rat -> Integer
numerator (Rat a _) = a


