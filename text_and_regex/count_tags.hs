{-# LANGUAGE OverloadedStrings, QuasiQuotes ,FlexibleContexts, FlexibleInstances #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Regex.PCRE.Heavy

main :: IO()
main = do
	text <- TIO.readFile "L1.html"
	let num_of_tags = T.count "<" text
	let num_of_tags1 = T.foldl (\x a -> if a == '<' then (x+1) else x) 0 text
	let tag_check = [re|<|]
	let res = scan tag_check text
	print $ "There are approximately " ++ (show $ num_of_tags) ++ " tags in the file"
	putStrLn $ "or another way to count it " ++ (show $ num_of_tags1)
	putStrLn $ "or with regex " ++ (show $ length res) ++ "!"