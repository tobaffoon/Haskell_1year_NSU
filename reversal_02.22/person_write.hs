import Person
import System.IO

p1 :: Person
p1 = Person "Harry" 44 86.3

--Writes info about Person p1 in "personInfoWrite.txt"
main :: IO ()
main = do
	fi <- openFile "personInfoWrite.txt" WriteMode
	fo -> openFile "myOut.txt" ReadMode
	hPutStrLn fi (show p1)
	hClose fi