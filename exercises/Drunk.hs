module Drunk
    (atIndices
    , everyOther
    , disjoint
    , stretch
    , drunk
    ) where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import MyList

-- example:
-- atIndices [1,4,5] "Tchauzinho"
-- = "cuz"
atIndices :: Integral i => [i] -> [a] -> [a]
atIndices = undefined
--atIndices _  []         = []
--atIndices [] _          = []
--atIndices (x:xs) (y:ys) =  ys !! x : atIndices xs ys

-- example:
-- everyOther 2 "Hello There"
-- = "HloTee"
everyOther :: Integral i => i -> [a] -> [a]
everyOther = undefined

-- examples:
-- disjoint [1,5,9] [2 .. 6]
-- = False
-- disjoint [1,5,9] [2,4 ..]
-- = True
-- ASSUMPTIONS FOR disjoint xs ys:
--   xs and ys are sorted
disjoint :: Ord a => [a] -> [a] -> Bool
disjoint = undefined

-- example:
-- stretch 3 "Gustavo"
-- = "GGGuuussstttaaavvvooo"
stretch :: Integral i => i -> [a] -> [a]
stretch = undefined

-- example:
-- drunk 3 "Gustavo"
-- = "GusGtuasvtoavo"
-- drunk 5 "Gustavo"
-- = "GustaGvuostavo"
-- To understand these string, either get drunk or look at the markings:
--       , , , , ,,,
--   "GusGtuasvtoavo"
--    ''' ' ' ' '
--         , , ,,,,,
--   "GustaGvuostavo"
--    ''''' ' '

alternate :: [a] -> [a] -> [a]
alternate _ []          = []
alternate [] _          = []
alternate (x:xs) (y:ys) = x : y : (alternate xs ys)

drunk :: Integral i => i -> [a] -> [a]
drunk i xs = begin ++ middle ++ end
    where 
        begin  = take i xs
        end    = drop (length xs - i) xs
        middle = alternate xs (drop i xs)

-- drunk' (x:xs) (y:ys) = y:x:(drunk' xs ys)


