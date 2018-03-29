module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral
    , Bool(..)
    , not
    , (&&)
    , (||)
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat = Zero | Succ Nat

instance Show Nat where

    show Zero     = "0"
    show (Succ n) = "S" ++ show n

instance Eq Nat where

    Zero   == Zero    = True
    Zero   == Succ _  = False
    Succ _ == Zero    = False
    Succ m == Succ n  = m == n

--x `lessThan` y  
--             | Succ x == y = True 
--             | otherwise = False
  
instance Ord Nat where

    (<=) x y  
           | Succ x == y = True 
           | otherwise = False
              
    -- Ord does not require defining min and max.
    -- Howevener, you should define them without using (<=).
    -- Both are binary functions: max m n = ..., etc.
    
    
    min x y 
           | x      == Zero = Zero
           | y      == Zero = Zero
           | Succ x == y    = x
           | Succ y == x    = y
             

    max x y
           | x == Zero || y == Zero = Zero
           | Succ x == y = x
           | Succ y == x = y

isZero :: Nat -> Bool
isZero Zero = True
isZero _    = False

pred :: Nat -> Nat
pred Zero     = Zero
pred (Succ x) = x

even :: Nat -> Bool
even Zero            = True
even (Succ Zero)     = False
even (Succ (Succ x)) = even x

odd :: Nat -> Bool
odd = not . even

-- addition
(<+>) :: Nat -> Nat -> Nat
(<+>) Zero x            = x
(<+>) x Zero            = x
(<+>) (Succ x) (Succ y) = (<+>) (Succ(Succ x)) y
          
          

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when subtraction returns a negative number.
(<->) :: Nat -> Nat -> Nat
(<->) Zero x            = Zero
(<->) x Zero            = x
(<->) (Succ x) (Succ y) = (<->) x y

-- multiplication
(<*>) :: Nat -> Nat -> Nat
(<*>) _ Zero     = Zero
(<*>) Zero _     = Zero
(<*>) (Succ x) y = (<+>) y ((<*>) x y)

-- exponentiation
(<^>) :: Nat -> Nat -> Nat
(<^>) _ Zero     = (Succ Zero)
(<^>) Zero _     = (Succ Zero)
(<^>) x (Succ y) = (<*>) x ((<^>) x y)

-- quotient
(</>) :: Nat -> Nat -> Nat
(</>) x (Succ Zero) = x
(</>) _ Zero        = error "Divisão por zero não permitida"
(</>) (Succ x) y = (<*>) y ((</>) x y)

-- remainder
(<%>) :: Nat -> Nat -> Nat
(<%>) = undefined

-- divides
(<|>) :: Nat -> Nat -> Bool
(<|>) = undefined

divides = (<|>)


-- x `absDiff` y = |x - y|
-- (Careful: here this - is the real minus operator!)
absDiff :: Nat -> Nat -> Nat
absDiff x y = if x <= y 
              then y - x 
              else x - y
            

(|-|) = absDiff

factorial :: Nat -> Nat
fractorial Zero     = (Succ Zero)
factorial (Succ x) = (<*>) (Succ x) (factorial x)

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg = undefined
-- | x == Zero = Zero
-- |(<=) x Zero = -1
-- | otherwise =  1

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo = undefined

-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!

toNat :: Integral a => a -> Nat
toNat 0 = Zero
toNat x = Succ (toNat (x - 1))

fromNat :: Integral a => Nat -> a
fromNat Zero = 0
fromNat a    = 1 + fromNat(pred(a))

--we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
        | x < 0     = undefined
        | x == 0    = undefined
        | otherwise = undefined

