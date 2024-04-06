import PersonInstances
import System.IO

--reads contents in "personInfoInst.txt" and copies them in "personInfoInstWrite.txt", but now as if they were entries in some data base
main :: IO ()
main = do
--fi can either be "personInfoInst.txt" or "personInfoInst1.txt"
--the only difference is the order of lines which doesn't matter if we use regexes
--So no matter what file we choose, info in "personInfoDBWrite.txt" will stay the same
--There are two files to show that the order of field can be arbitrary
	fi <- openFile "personInfoInst.txt" ReadMode
	fo <- openFile "personInfoInstWrite.txt" WriteMode
	text <- hGetContents fi
	let newPerson = read text :: Person
	hPutStrLn fo (show newPerson)
	hClose fo
	hClose fi