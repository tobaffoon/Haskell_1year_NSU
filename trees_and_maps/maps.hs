import qualified Data.List as L
import qualified Data.Map as M

data Gender = F | M deriving (Show, Read, Eq)

listName = ["Helga", "Eugenia", "Kate", "Eugene", "Paul", "Alexander", "Alexandra", "Marie", "Hope", "Julie"] :: [String]
listID = [131, 132, 134, 135, 136, 137, 138, 139, 140, 141] :: [Int]
listGender = [F, F, F, M, M, M, F, F, F, F] :: [Gender]
listAge = [17, 18, 17, 19, 18, 19, 21, 19, 18, 20] :: [Int]
listFrends = [True, False, True, False, False, True, False, True, True, False] :: [Bool]

--I'm not sure how to creat big tuples more effectively
mapInfo = M.fromList [(listID !! a, (listName !! a, listGender !! a, listAge !! a, listFrends !! a)) | a <- [0..L.length listName-1]]

findYoungMen :: M.Map Int (String, Gender, Int, Bool) -> M.Map Int String
findYoungMen m = toName $ M.filter (isYoungMen) m

findYoungMenID :: M.Map Int (String, Gender, Int, Bool) -> [Int]
findYoungMenID m = M.keys $ findYoungMen m

isYoungMen :: (String, Gender, Int, Bool) -> Bool
isYoungMen (_,g,_,_) = g == M

toName :: M.Map Int (String, Gender, Int, Bool) -> M.Map Int String
toName m = fmap (\(s,_,_,_) -> s) m
{-
    findYoungMen mapInfo 
    fromList [(135,"Eugene"),(136,"Paul"),(137,"Alexander")]

    findYoungMenID mapInfo 
    [135,136,137]
-}