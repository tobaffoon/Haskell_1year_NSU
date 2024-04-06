{-fibc :: Integer -> (Integer, Integer)
fibc 1 = (1,0)
fibc n = (prev1, prev2) where
    s=snd(fibc (n-1))
    f=fst(fibc (n-1))
    prev1 = 2*s+f
    prev2 = f
fibOld n = fst(fibc n)-}
fiblist = 1 : 1 : [ a+b | (a,b) <- zip fiblist (tail fiblist) ]
fibFromFirst :: Integer -> Integer -> Integer -> Integer
fibFromFirst n1 n2 1 = 1
fibFromFirst n1 n2 n = if n==2
                    then n1 + n2 
                    else fibFromFirst n2 (n1 + n2) (n - 1)
fib n = fibFromFirst 0 1 n
golden n = fromIntegral (fib (n + 1)) / fromIntegral (fib n)
--fib n = writeFile "./input.txt" (show (fibFromFirst 0 1 n))
--1000000 - 36 sec
--5000000 - 14 min 11 sec