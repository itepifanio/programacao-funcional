import Prelude hiding (length, drop, init)
-- 1)
pow :: Int -> Int -> Int
pow x 0 = 1
pow x y = x * pow x (y-1)

-- 2)
length []     = 0
length (x:xs) = 1 + length xs

drop a []     = []
drop 0 (x:xs) = (x:xs)
drop a (x:xs) = drop (a-1) xs

init []     = []
init [x]    = []
init (x:xs) = x : init xs
