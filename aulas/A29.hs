-- Definir <$ usango fmap

-- fmap :: Functor f => (a -> b) -> f a -> f b

badmap :: (a -> b) -> [a] -> [b]
badmap f []   = []
badmap (x:xs) = badmap f xs ++ [f x]

(<$) :: a -> f b -> f a
(<$) = undefined
