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
                       (7,2)
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
         (ALeaf (307,3))
     )
	 
eGuard :: Bool -> String -> Either String ()
eGuard False ctx = Left ("smth. wrong with ID " ++ ctx)
eGuard True _ = Right ()
	 
--main function run this
treeProd :: ATree (Int, Double) -> Either String Double
treeProd (ALeaf (idx, x)) = do
	eGuard (x /= 0) (show idx)
	return x

treeProd (ABranch l (idx, x) r) = do
	eGuard (x /= 0) (show idx)
	l_val <- treeProd l
	r_val <- treeProd r
	return (l_val * x * r_val)
	 
divWithTreeE :: Double -> ATree (Int,Double) -> Either String Double
divWithTreeE num tr = do
	prod <- (treeProd tr)
	return (num * prod)