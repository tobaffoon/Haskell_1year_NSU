import Control.Monad

data ATree a = ALeaf a | ABranch (ATree a) a (ATree a) deriving (Show, Read)

dTree :: ATree Double
--dTree = ABranch (ABranch (ALeaf 21) 2.2 (ABranch (ABranch (ALeaf 62) 0 (ALeaf 66)) 22 (ALeaf 46))) 1.1 (ABranch (ALeaf 34) 3.3 (ALeaf 35))
dTree = ABranch (ABranch (ALeaf 2) 2 (ABranch (ABranch (ALeaf 2) 2 (ALeaf 2)) 2 (ALeaf 2))) 2 (ABranch (ALeaf 2) 2 (ALeaf 2))

--main function run this
numTree :: ATree Double -> ATree (Int,Double)
numTree x = snd $ numTree' (1, x)

numTree' :: (Int, ATree a) -> (Int, ATree (Int, a))
numTree' (acc, ALeaf arg) = (acc + 1, ALeaf (acc, arg))
	
numTree' (acc, (ABranch l m r)) = (rid, ABranch l_tree m_tree r_tree)
	where	
			m_tree = (acc, m)
			(lid, l_tree) = numTree' (acc + 1, l)
			(rid, r_tree) = numTree' (lid, r)
			
getTreeVal :: ATree a -> a
getTreeVal (ALeaf x) = x
getTreeVal (ABranch _ x _) = x