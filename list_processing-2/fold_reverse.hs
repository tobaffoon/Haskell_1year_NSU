reverse' :: [a] -> [a]
reverse' (x:xs) = foldl (\acc x-> x : acc) [x] xs
