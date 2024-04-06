import Prelude hiding ((!!), reverse, init, cycle, (++), take, elem)
(++) :: [a] -> [a] -> [a]
xs ++ ys = if null xs
            then ys
            else head xs : (tail xs ++ ys)
{-
[1,2] ++ [3..10]
[1,2,3,4,5,6,7,8,9,10]

"privet," ++ " vladimir nikolaevich"
"privet, vladimir nikolaevich"
-}