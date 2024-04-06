import Person
import System.IO

--reads contents in "personInfo.txt" and copies them in "personInfoWrite.txt"
main :: IO ()
main = do
	fi <- openFile "personInfo.txt" ReadMode
	fo <- openFile "personInfoWrite.txt" WriteMode
	text <- hGetContents fi
	let p2 = read text :: Person
	hPutStrLn fo (show p2)
	hClose fo
	hClose fi