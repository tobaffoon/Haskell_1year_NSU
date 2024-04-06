import Data.Maybe
data ATree a = Lf a | Br (ATree a) a (ATree a)
    deriving (Show, Read, Eq)
tree :: ATree Integer
tree = Br
        (
            Br
                (Lf 51)
                40
                (Lf 26)
        )
            35
        (
            Br
            (
                Br
                    (Lf 20)
                    63
                    (Lf 45)
            )
                22
                (Lf 80)
        )

t1 = Br
    (
        Br
            (Lf 20)
            63
            (Lf 45)
    )
        22
        (Lf 80)

t2 = Br
        (Lf 20)
        63
        (Lf 45)

t3 = Lf 45

fromATree :: ATree Integer -> Integer
fromATree (Lf a) = a
fromATree (Br _ a _) = a

isChild :: ATree Integer -> ATree Integer -> Bool   --I'm sorry it's unreadable again
_ `isChild` Lf x = False
(Lf child) `isChild` (Br (left) x (rigth)) = (child == (fromATree left)) || (child == (fromATree rigth))
(Br (cleft) x (crigth)) `isChild` (Br (pleft) _ (prigth)) = (x == fromATree pleft && cleft `isChild` pleft && crigth `isChild` pleft) || (x == fromATree prigth && crigth `isChild` prigth && crigth `isChild` prigth)

parent :: ATree Integer -> ATree Integer -> Maybe (ATree Integer)
parent (Lf x) _ = Nothing
parent (Br (pleft) x (prigth)) child = if child `isChild` (Br (pleft) x (prigth))   --lines 55 to 59 are somewhat unoptimized as we run "parent" for a couple of times
    --If it's already child we keep it
    then Just (Br (pleft) x (prigth))
    --I'm not sure if it's possible to avoid calling parent twice
    else if isJust (parent pleft child)
        then parent pleft child
        else parent prigth child
{-
parent tree (Lf 45)
Just (Br (Lf 20) 63 (Lf 45))

parent tree t1   
Just (Br (Br (Lf 51) 40 (Lf 26)) 35 (Br (Br (Lf 20) 63 (Lf 45)) 22 (Lf 80)))

parent tree t2
Just (Br (Br (Lf 20) 63 (Lf 45)) 22 (Lf 80))

parent tree tree
Nothing
-}