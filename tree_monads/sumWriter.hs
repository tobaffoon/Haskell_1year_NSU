import Control.Monad.Writer

data ATree a = ALeaf a | ABranch (ATree a) a (ATree a) deriving (Show, Read)

idTree :: ATree (Int,Double)
idTree =
    ABranch
       (
         ABranch
            (ALeaf (4,21))
              (2,2.2)
            (
              ABranch
                 (
                   ABranch
                     (ALeaf (100,62))
                       (7,1)
                     (ALeaf (200,66))
                 )
                     (5,22)
                  (ALeaf (203,46))
            )

       )
             (1,1.1)
     (
       ABranch
         (ALeaf (301,34))
            (3,3.3)
         (ALeaf (307,1))
     )
	 
--main function run this
sumWithTree :: ATree (Int, Double) -> IO ()
sumWithTree = getLog . sumWithTreeE
	
sumWithTreeE :: ATree (Int, Double) -> Writer String Double
sumWithTreeE (ALeaf (idx, x)) = do
	writer (x, "worked through ID " ++ show idx ++ "; \n")
	return x

sumWithTreeE (ABranch l (idx, x) r) = do
	l_val <- sumWithTreeE l
	r_val <- sumWithTreeE r
	writer (x, "worked through ID " ++ show idx ++ "; \n")
	return (l_val + x + r_val)
	
getLog :: Writer String a -> IO ()
getLog log = putStr $ snd $ runWriter log