cycleshift :: [a] -> [a]
cycleshift (x:xs) = xs ++ [x]

rotateR :: [a] -> [[a]]
rotateR xs = foldr (\x ys -> [cycleshift $ head ys] ++ ys) [xs] [1..(length xs-1)]

rotateL :: [a] -> [[a]]
rotateL xs = foldl (\ys x -> [cycleshift $ head ys] ++ ys) [xs] [1..(length xs-1)]

{-
rotateL [1..4]
[[4,1,2,3],[3,4,1,2],[2,3,4,1],[1,2,3,4]]

rotateR [2..5]
[[5,2,3,4],[4,5,2,3],[3,4,5,2],[2,3,4,5]]

rotateL "aleksei"
["ialekse","eialeks","seialek","kseiale","ekseial","lekseia","aleksei"]

rotateR "vladimir"
["rvladimi","irvladim","mirvladi","imirvlad","dimirvla","adimirvl","ladimirv","vladimir"]

rotateL []
[[]]
-}