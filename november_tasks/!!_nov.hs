import Prelude hiding ((!!), reverse, init, cycle, (++), take, elem)
(!!) :: [a] -> Int -> a
xs !! n = if (null xs || n < 0)
            then error "no element with such index"
            else if n == 0
                 then head xs
                 else (tail xs) !! (n-1)
{-
"hello, dear user" !! 12
'u'

"hello, dear user" !! 16
*** Exception: no element with such index
CallStack (from HasCallStack):
  error, called at !!_nov.hs:4:18 in main:Main

[1..] !! 90             
91

[] !! 0
*** Exception: no element with such index
-}