module A14 where

import Prelude hiding ( until )

until :: (a -> Bool) -> (a -> a) -> a -> a
until b p i = if b i
              then i
              else until b p (p i)
