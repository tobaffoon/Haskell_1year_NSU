import Barans
import Data.List (find)
import Control.Monad (mplus)

selected_barans = ["i3", "i5", "i6", "i9", "i12"]
{--
                      i12
          i10, i11
      i8           i9         i6
   i3    i7     i3   i5    i4    i5
i1    i2

--}

--task 1 - find grandfather on mom's side
mgrandfather :: Sheep -> Maybe Sheep

mgrandfather s = father =<< mother s 

{-
mgrandfather "i12"
Just "i9"

mgrandfather "i3"
Nothing
-}

--task 2 - find grandgrandfather on grandfather's side on mom's side
ggrandfather :: Sheep -> Maybe Sheep

ggrandfather s = father =<< father =<< mother s

{-
ggrandfather "i12"
Just "i5"

ggrandfather "i10"
Nothing
-}

--task 3.a - find all parents
extractMaybeList :: Maybe a -> [a]

extractMaybeList Nothing = []
extractMaybeList (Just x) = [x]

makeMaybeList :: [Maybe a] -> [a]

makeMaybeList [] = []
makeMaybeList (m:ms) =	extractMaybeList m ++ makeMaybeList ms
							
parents :: Sheep -> [Sheep]

parents s = (makeMaybeList $ [mother s]) ++ (makeMaybeList $ [father s])

{-
parents "i12"
["i11","i6"]

parents "i2"
[]
-}

--task 3.b - find all grandparents
maybeParents:: Maybe Sheep -> [Sheep]

maybeParents Nothing = []
maybeParents (Just s) = parents s

grandparents :: Sheep -> [Sheep]

grandparents s = (maybeParents $ mother s) ++ (maybeParents $ father s)

{-
grandparents "i12"
["i8","i9","i4","i5"]

grandparents "i3"
[]

grandparents "i8"
["i1","i2"]
-}

--task 4 - find if sheep is an orphan
isOrphan :: Sheep -> Bool

isOrphan s = null $ parents s

{-
isOrphan  "i8"
False

isOrphan  "i1"
True
-}

--task 5 - find selected father
isEq :: Eq a => a -> Maybe a -> Bool

_ `isEq` Nothing = False
n `isEq` (Just x) = x == n

fatherSel :: Sheep -> Maybe Sheep

fatherSel s = find ( `isEq` (father s)) selected_barans

{-
fatherSel "i12"
Just "i6"

fatherSel "i3"
Nothing

fatherSel "i2"
Nothing
-}

--task 6 - find closest selected relative on father's side
fSelRelative :: Sheep -> Maybe Sheep

fSelRelative s = fSelRelative' (father s) where
	fSelRelative' :: Maybe Sheep -> Maybe Sheep
	
	fSelRelative' Nothing = Nothing
	fSelRelative' (Just fs) = (find ( ==fs) selected_barans) `mplus` (fSelRelative fs)			--if current father is selected he will be chosen, otherwise we will check his father
	
{-
fSelRelative "i12"
Just "i6"

--No i6 in selected_barans
fSelRelative "i12"
Just "i5"

--No i6 and i5 in selected_barans
fSelRelative "i12"
Nothing

fSelRelative "i1"
Nothing
-}