import Control.Monad
import Control.Monad.State

data ATree a = ALeaf a | ABranch (ATree a) a (ATree a) deriving (Show, Read)

dTree :: ATree Double
--dTree = ABranch (ABranch (ALeaf 21) 2.2 (ABranch (ABranch (ALeaf 62) 0 (ALeaf 66)) 22 (ALeaf 46))) 1.1 (ABranch (ALeaf 34) 3.3 (ALeaf 35))
dTree = ABranch (ABranch (ALeaf 2) 2 (ABranch (ABranch (ALeaf 2) 2 (ALeaf 2)) 2 (ALeaf 2))) 2 (ABranch (ALeaf 2) 2 (ALeaf 2))

--main function run this
numTree :: ATree Double -> ATree (Int,Double)
numTree x = evalState (enumS x) 1

enumS :: ATree Double -> State Int (ATree (Int,Double))

enumS (ALeaf x) = do
	num <- get
	put (num + 1)
	return (ALeaf (num, x))
	
enumS (ABranch l x r) = do
	num <- get
	put (num + 1)
	l_num <- enumS l
	r_num <- enumS r
	return (ABranch l_num (num, x) r_num)