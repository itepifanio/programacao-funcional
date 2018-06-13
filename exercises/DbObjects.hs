module DbObjects where

data DbObject = DbString String
              | DbInt Int
              | DbBool Bool
              deriving (Eq, Show)

type Database = [DbObject]

exampleDB :: Database
exampleDB =
    [ DbString "oi"
    , DbInt 12
    , DbBool False
    , DbInt 28
    , DbBool True
    , DbInt 6
    , DbString " "
    , DbBool False
    , DbString "mundo"
    , DbInt (-4)
    , DbString ""
    ]

-- counts outputs the concatenation of all strings
-- and the sum of all ints that appear in its input
-- example:
-- counts exampleDB
-- > ("oi mundo",42)

counts :: Database -> (String, Int)

counts [] = ("", 0)

counts (DbString s : objs) = (s ++ s', n')
    where (s',n') = counts objs

counts (DbInt n : objs) = (s', n + n')
    where (s',n') = counts objs

counts (_ : objs) = counts objs


-- implement counts as a fold

countsF :: Database -> (String, Int)
countsF = undefined

