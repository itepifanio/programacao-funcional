module ExFun where

import Prelude hiding ( exp, pred )

-- Nat datatype --------------------------------

data Nat = Z | S Nat
     deriving (Eq, Show)

instance (Num Nat) where
    (+) = add
    (*) = mul
    abs = id
    fromInteger 0 = Z
    fromInteger n
      | n > 0     = S $ fromInteger (n-1)
      | otherwise = Z
    signum Z = Z
    signum n = S Z
    negate n = Z

instance (Ord Nat) where
    Z     <= m     = True
    (S n) <= Z     = False
    (S n) <= (S m) = n <= m

{- definitions of add, mul, exp: 

add n Z     = n
add n (S m) = S (add m n)

mul n Z     = Z
mul n (S m) = add (mul n m) n

exp n Z     = S Z
exp n (S m) = mul (exp n m) n

-}

------------------------------------------------

pred :: Nat -> Nat
pred Z     = Z
pred (S x) = x

toNat :: Integral a => a -> Nat
toNat 0 = Z
toNat x = S (toNat (x - 1))

fromNat :: Integral a => Nat -> a
fromNat Z = 0
fromNat a    = 1 + fromNat(pred(a))

------------------------------------------------

add x y = fun 1 x y
mul = fun undefined
exp = fun undefined

fun :: Integral i => i -> (Nat -> Nat -> Nat)
-- add
fun 1 n Z = n
fun 1 (S x) (S y) = fun 1 (S(S x)) y

