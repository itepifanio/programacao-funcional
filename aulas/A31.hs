class Functor (f :: * -> *) where

fmap :: (a -> b) -> f a -> f b
fmap = undefined

-- Laws:
-- fmap id = id
-- fmap (f . g) = fmap f . fmap g

-- for free
(<$>) :: (a -> b) -> f a -> f b
(<$>) = fmap

(<$) :: a -> f b -> f a
x <$ cs = fmap (const x) cx

void :: f a -> f ()
void = (() <$)

{- class Functor f => Applicative (f :: * -> *) where

infixl 4 <*>, <*, *>, <**>

pure :: a -> f a

instance Applicative Maybe where
pure :: a -> Maybe a
pure = Just

(<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
Just g <*> Just x = Just (g x)
_      <*> _      = Nothing


(<*>) :: f (a -> b) -> f a -> f b

-- Laws:
--

-- for free
(*>) :: f a -> f b -> f b
(<*) :: f a -> f b -> f a
liftA :: (a -> b) -> f a -> f b
liftA = fmap -- for free from Functor

-}

-- Aula A32
instance Functor (Either e) where
    fmap :: (a -> b) -> (Either e a) -> (Either e b)
    fmap f (Left x)  = Left  x
    fmap f (Right x) = Right (f x)

instance Semigroup Bool where
    (<>) = (&&)

instance Semigroup Int where
    (<>) = (+)

instance Semigroup String where
    (<>) = (++)

instance Semigroup [a] where
    (<>) = (++)

class Semigroup a where
    (<>) :: a -> a -> a

    sconcat :: NonEmpty a -> a
    sconcat (a :| as) = go a as where
        go b (c:cs) = b <> go c cs
        go b []     = b



