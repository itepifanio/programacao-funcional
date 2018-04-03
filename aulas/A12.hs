module A12 where

matheus :: Num a => [a] -> a
matheus (_:x:y:z:xs) = matheus (x:y:z:xs)
matheus xs           = sum xs
