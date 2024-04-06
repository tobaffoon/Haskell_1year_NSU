import Prelude hiding ((!!), reverse, init, cycle, (++), take, elem)
(++) :: [a] -> [a] -> [a]
xs ++ ys = if null xs
            then ys
            else head xs : (tail xs ++ ys)

subsequences1 :: [a] -> [[a]]                                       --for sure it covers all the subsequences, some of them just occur more than once
--we make two lists: in the first we place the result, in the second - the part of list that we will change
subsequences1 (x : xs) = subs [x] xs where     
    subs :: [a] -> [a] -> [[a]]
--if result is complete we add it
    subs ys [] = [ys]
--the first part makes subs without the result part (so we can make subs wihout the first, the first & second etc. element)
--the second part lets us remove the first element of "buffer" (so that we can make subs without second or third or so on element)
--the third part just builds up the result from buffer (thus giving us the full list and all the lists that we got at any other step)
    subs (y : ys) (y1 : ys1) = subs [y1] ys1 ++ subs (y : ys) ys1 ++ subs ((y : ys) ++ [y1]) ys1 

subsequences :: [a] -> [[a]]
subsequences [] = [[]]
--the first part builds subs without the first elem, the second - with some element and all the elems after him
subsequences (x : xs) = subsequences xs ++ [x : a | a <- subsequences xs]

{-
subsequences1 [1..4]
[[4],[3],[3,4],[4],[2],[2,4],[4],[2,3],[2,3,4],[4],[3],[3,4],[4],[1],[1,4],[4],[1,3],[1,3,4],[4],[3],[3,4],[4],[1,2],[1,2,4],[4],[1,2,3],[1,2,3,4]]

subsequences [1..4] 
[[],[4],[3],[3,4],[2],[2,4],[2,3],[2,3,4],[1],[1,4],[1,3],[1,3,4],[1,2],[1,2,4],[1,2,3],[1,2,3,4]]

subsequences "abc" 
["","c","b","bc","a","ac","ab","abc"]

subsequences "a"  
["","a"]
-}