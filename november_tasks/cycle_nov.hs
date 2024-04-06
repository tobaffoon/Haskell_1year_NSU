import Prelude hiding ((!!), reverse, init, cycle, (++), elem)
(++) :: [a] -> [a] -> [a]
xs ++ ys = if null xs
            then ys
            else head xs : (tail xs ++ ys)
            
cycle :: [a] -> [a]
cycle xs = if null xs
            then error "Empty list!"
            else xs ++ cycle xs

{-
take 10 $ cycle [1..3]
[1,2,3,1,2,3,1,2,3,1]

take 15 $ cycle "again"
"againagainagain"

cycle []
*** Exception: Empty list!
-}