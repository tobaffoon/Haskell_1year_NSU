import Prelude hiding ((!!), reverse, init, cycle, (++), take, elem)
init :: [a] -> [a]
init xs = if null xs
            then error "Empty list!"
            else if null $ tail xs
                then []
                else head xs : (init $ tail xs)
                
init1 :: [a] -> [a]
init1 [] = error "Empty list!"
init1 [x] = []
init1 (x:xs) = x : init1 xs

{-
init [1,3..15]
[1,3,5,7,9,11,13]

init1 "hello!"
"hello"

init []       
*** Exception: Empty list!

init "a"
""
-}