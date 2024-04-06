{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO()
main = do
	text <- TIO.readFile "L1.html"
	let text_lines = T.lines text
	let striped_text = [text_lines !! a | a <- [i*3-1 | i <- [1..length text_lines `div` 3]]]
	TIO.writeFile "L13.html" (T.unlines striped_text)