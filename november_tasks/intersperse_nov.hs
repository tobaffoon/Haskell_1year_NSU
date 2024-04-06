intersperse :: a -> [a] -> [a]               
intersperse ch xs = foldr (\x y -> if null y then x : y else x : ch : y) [] xs

{-
intersperse 1 [2,4..20]
[2,1,4,1,6,1,8,1,10,1,12,1,14,1,16,1,18,1,20]

intersperse 0 [1,-1,1,-1]
[1,0,-1,0,1,0,-1]

intersperse ' ' "Imme"
"I m m e"

intersperse ',' []
""
-}