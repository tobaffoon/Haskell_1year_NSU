howmany :: Eq a => a -> [a] -> Int
howmany x xs = foldr (\a b -> if a == x then 1 + b else b) 0 xs

{-
howmany '8' "89386480088"
5

howmany 1 [1,2,3,1,2,3,1,2,3]
3

howmany ' ' ""
0
-}

howmany_g_b_letters :: [Char] -> (Int,Int)
howmany_g_b_letters xs = foldr (\a ans -> if a == 'a' || a == 'e' || a == 'i' || a == 'o' || a == 'u' then (fst ans + 1, snd ans) else if a == 't' || a == 'n' || a == 'r' || a == 's' || a == 'h' then (fst ans, snd ans + 1) else (fst ans, snd ans)) (0,0) xs
{-
howmany_g_b_letters "hola!"
(2,1)
--o,a & h

howmany_g_b_letters "aloha!"
(3,1)
--a,o,a & h

howmany_g_b_letters "bon jour!"
(3,2)
--o,o,u & n,r

howmany_g_b_letters "privet!"  
(2,2)
--i,e & r,t

howmany_g_b_letters  ""    
(0,0)
-}