import Prelude hiding ((!!), reverse, init, cycle, (++), take, elem)
elem :: Eq a => a -> [a] -> Bool
x `elem` xs = if null xs
                then False
                else if head xs == x
                    then True
                    else x `elem` (tail xs)
{-
'x' `elem` "find x"
True

'y' `elem` "find x"
False

'x' `elem` []
False
-}