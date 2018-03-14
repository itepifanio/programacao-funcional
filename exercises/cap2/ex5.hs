myInit list = take (length list - 1) list

myOtherInit (list:n) = if null n 
                       then []
                       else list : myOtherInit n

nthElement :: [a] -> Int -> Maybe a 
nthElement [] a = Nothing
nthElement (x:xs) a = if a  < 1 then Nothing else
                      if a == 1 then Just x
                      else nthElement xs (a-1)
