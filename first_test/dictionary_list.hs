listToListList :: [a] -> [[a]]
listToListList xs = [[x] | x <- xs]

lst :: String -> [String]
lst base = lst1 (listToListList base) where
    lst1 :: [String] -> [String]
    lst1 str = str ++ lst1 [l : x | l <- base, x <- str]