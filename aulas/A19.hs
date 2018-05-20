module A19 where

import Prelude hiding (exp)

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition f xs = (ys, zs)
    where
        ys = filter f xs
        zs = filter (not . f) xs


