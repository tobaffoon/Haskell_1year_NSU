import Control.Monad

data ATree a = ALeaf a | ABranch (ATree a) a (ATree a) deriving (Show, Read)

dTree :: ATree Double
--dTree = ABranch (ABranch (ALeaf 21) 2.2 (ABranch (ABranch (ALeaf 62) 0 (ALeaf 66)) 22 (ALeaf 46))) 1.1 (ABranch (ALeaf 34) 3.3 (ALeaf 35))
dTree = ABranch (ABranch (ALeaf 2) 2 (ABranch (ABranch (ALeaf 2) 2 (ALeaf 2)) 2 (ALeaf 2))) 2 (ABranch (ALeaf 2) 2 (ALeaf 2))

--main function run this
treeProd :: ATree Double -> Maybe Double
treeProd (ALeaf x) = do
	guard (x /= 0)
	return x

treeProd (ABranch l x r) = do
	guard (x /= 0)
	l_val <- treeProd l
	r_val <- treeProd r
	return (l_val * x * r_val)
	
divWithTreeM :: Double -> ATree Double -> Maybe Double
divWithTreeM num tr = do
	prod <- treeProd tr
	return (num / prod)