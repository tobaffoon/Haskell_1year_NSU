{-# LANGUAGE QuasiQuotes, FlexibleContexts, OverloadedStrings #-}

import Text.Regex.PCRE.Heavy
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.IO as S

main :: IO()
main = do
	fi <- S.openFile "L1.html" S.ReadMode
	fo <- S.openFile "urls.html" S.WriteMode
	text <- TIO.hGetContents fi
	let f .> g = g . f
	let
		url_check :: Regex
		url_check = [re|<a href=\"(.+?)\".+?</a>|]
		--find something in a tag and extract text in quotes in not gready way
	let 
		urls :: [T.Text]
		urls = map (snd .> head) (scan url_check text)
		--as scan returns [(a, [a])] where [a] is contents of groups and it is the contents of group that we are interested interested
		--first we take the lists of contents of groups, then take the first group (with url)
	let
		print_text_list :: [T.Text] -> IO ()
		print_text_list xs = mapM_ (TIO.hPutStrLn fo) xs
	print_text_list urls
	S.hClose fi
	S.hClose fo