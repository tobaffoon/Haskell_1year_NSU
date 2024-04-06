import Prelude hiding ((!!), reverse, init, cycle, (++), take, elem)
(++) :: [a] -> [a] -> [a]
xs ++ ys = if null xs
            then ys
            else head xs : (tail xs ++ ys)
            
permutations :: [a] -> [[a]]
permutations xs = perm xs [] where
    perm :: [a] -> [a] -> [[a]]
    perm []  _ = [[]]
    perm [y] ys = map (y :) $ perm ys []                            --makes a list of permutations with sublist and fixed leading y
    perm (y : ys) ys1 = perm [y] (ys ++ ys1) ++ perm ys (y : ys1)   --first part starts the line above, second part will start the line above, but with the second element as the leading one

{-
permutations "vlas"
["vlas","vlsa","vasl","vals","vsal","vsla","lasv","lavs","lsva","lsav","lvsa","lvas","aslv","asvl","alvs","alsv","avls","avsl","salv","savl","slva","slav","svla","sval"] 

permutations [1,2,3]
[[1,2,3],[1,3,2],[2,3,1],[2,1,3],[3,2,1],[3,1,2]]

permutations []
[[]]
-}