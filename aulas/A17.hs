import Data.List

--module A17 where


insert' :: Int -> a -> [a] -> [a]
-- inser 2 'G' "UST" -> "USGT"
insert' n x xs = undefined

-- 'G' "UST" -> ["GUST", "UGST", "USGT", "USTG"]
interpose :: a -> [a] -> [[a]]
interpose w []          = [[w]]
interpose w all@(x:xs)  = (w:all) : map (x:) (interpose w xs)
--(w : all) : interpose w xs

-- permutations' (x:xs)
-- map (f x) (permutations' xs)
permutations' :: [a] -> [[a]]
permutations' [] = [[]]
permutations' (x:xs) = concat $ map (interpose x) (permutations' xs)

