module JPHalve where

-- primitive halve
-- solution by João Pedro Holanda

jphalve :: [a] -> ([a],[a])
jphalve xs = jphalve' xs xs

jphalve' :: [a] -> [a] -> ([a],[a])
jphalve' xs []  = ([],xs)
jphalve' xs [_] = ([],xs)
jphalve' (x:xs) (_:_:ys) = (x:pre,suf)
    where
      (pre,suf) = jphalve' xs ys

