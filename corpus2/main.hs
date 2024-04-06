{-# LANGUAGE QuasiQuotes, FlexibleContexts, OverloadedStrings #-}

import Text.Regex.PCRE.Heavy
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.IO as S

main :: IO()
main = do
	fi <- S.openFile "corpus2.txt" S.ReadMode
	subwords <- S.openFile "subwords.txt" S.WriteMode
	cores <- S.openFile "cores.txt" S.WriteMode
	swap_fi <- S.openFile "corpus2new.txt" S.WriteMode
	text <- TIO.hGetContents fi
	let ls = T.lines text
	let 
		fst_rgx :: Regex
		fst_rgx = [re|(1+)([^19]+)(9+)|]
	let
		not_null :: [[a]] -> [[a]]
		not_null xs = filter (not . null) xs
	let
		rgx_matches :: Regex -> [T.Text] -> [[(T.Text, [T.Text])]]
		rgx_matches rx tx = map (scan rx) tx 
	let
		subw :: [[(T.Text, [T.Text])]] -> [T.Text]
		subw tx = foldl (\acc l -> acc ++ (map fst l)) [] (not_null tx)
	let
		getgroup :: [[(T.Text, [T.Text])]] -> Int -> [T.Text]
		getgroup tx n = foldl (\acc l -> acc ++ (map (\pair -> (snd pair) !! n) l)) [] (not_null tx)
	let 
		swapgroups :: [[(T.Text, [T.Text])]] -> Int -> Int -> [T.Text]
		swapgroups tx n m = [sub [re|fst_grp !! i|] (snd_grp !! i) (sub [re|snd_grp !! i|] (fst_grp !! i) (initial !! i)) | i <- [0..length (not_null tx) - 1]] where
			initial = subw tx
			fst_grp = getgroup tx n
			snd_grp = getgroup tx m
	let
		fprint_line :: [T.Text] -> S.Handle -> IO ()
		fprint_line xs file = mapM_ (TIO.hPutStrLn file) xs 
	let 
		regFromStr :: String -> Regex
		regFromStr str = [re|str|]
	let res = rgx_matches fst_rgx ls
	print $ length $ not_null res
	print $ regFromStr "abc"
	print $ [re|abc|]
	fprint_line (subw res) subwords
	fprint_line (getgroup res 1) cores
	fprint_line (swapgroups res 0 2) swap_fi
	S.hClose fi
	S.hClose subwords
	S.hClose cores
	S.hClose swap_fi