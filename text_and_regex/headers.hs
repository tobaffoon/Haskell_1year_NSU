{-# LANGUAGE QuasiQuotes, FlexibleContexts, OverloadedStrings, FlexibleInstances #-}

import Text.Regex.PCRE.Heavy
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.IO as S
import qualified GHC.IO.Encoding as EIO

instance {-# OVERLAPPING #-} Show String where
show x = ['"'] ++ x ++ ['"']

main :: IO()
main = do
	fi <- S.openFile "L1.html" S.ReadMode
	fo <- S.openFile "headers.html" S.WriteMode
	cp <- EIO.mkTextEncoding "cp866"
	S.hSetEncoding fo EIO.utf8
	S.hSetEncoding fi EIO.utf8
	text <- TIO.hGetContents fi
	let f .> g = g . f
	let
		h_check :: Regex
		h_check = [re|<([hH][1-6])>.+?</\1>|]
		--it finds contents of headers and tag in not gready way
	let
		heads :: [T.Text]
		heads = map fst (scan h_check text)
	let
		print_text_list :: [T.Text] -> IO ()
		print_text_list xs = mapM_ (TIO.hPutStrLn fo) xs
	print_text_list heads
	S.hClose fi
	S.hClose fo