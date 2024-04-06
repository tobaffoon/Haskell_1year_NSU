import qualified Data.Map as M
import Data.Maybe
import Data.List (find)
data Tr = Nd Integer Integer | Lf
    deriving (Show, Read, Eq)

t1 = M.fromList [(43, Nd 62 66), (62, Lf), (66, Lf), (22, Nd 46 43), (46, Lf), (2, Nd 22 21), (21, Lf), (1, Nd 2 3), (35, Lf), (34, Lf), (3, Nd 35 34)]

t = M.fromList [(21, Nd 12 34), (12, Nd 51 26), (51, Lf), (26, Lf), (34, Nd 17 30), (17, Lf), (30, Nd 73 27), (73, Lf), (27, Lf)]
--this map is not a tree (has cycles)
tbad = M.fromList [(21, Nd 12 34), (12, Nd 51 26), (51, Lf), (26, Lf), (34, Nd 21 30), (17, Lf), (30, Nd 73 27), (73, Lf), (27, Lf)]

findRoot :: M.Map Integer Tr -> Tr
--it looks for the Tr with the found id. It gives the list of all children and list of all keys of Branches
findRoot mxs = fromJust (M.lookup (findRootId (inherits mxs) (M.keys (M.filter isNd mxs))) mxs)

findRootId :: [Integer] -> [Integer] -> Integer
--it finds id of the element which is in the list of all branches, but not a child (not in the list of children)
findRootId mys klist = fromJust (find (\a -> not(a `elem` mys)) klist)

--list of all children (without Leafs)
inherits :: M.Map Integer Tr -> [Integer]
inherits mxs = concatT (map (fromTr) (M.elems (M.filter isNd mxs)))

fromTr :: Tr -> [Integer]
fromTr Lf = []
fromTr (Nd a b) = [a,b]

isNd :: Tr -> Bool
isNd Lf = False
isNd (Nd a b) = True

concatT :: [[a]] -> [a]
concatT [] = []
concatT (x : xs) = x ++ (concatT xs)

{-
findRoot t
Nd 12 34

findRoot tbad
*** Exception: Maybe.fromJust: Nothing
-}

{-data ATree a = TLf a | TBr (ATree a) a (ATree a)
    deriving (Show, Read, Eq)
restoreTree :: M.Map Int Tr -> ATree Int
restoreTree mxs = 
    let root = findRoot mxs
    in buildTree (fromTr root !! 0) (findRootId mxs) (fromTr root !! 1) where
        buildTree ATree-} 