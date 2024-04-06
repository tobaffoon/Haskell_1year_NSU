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

fromATree :: ATree a -> a
fromATree (Lf a) = a
fromATree (Br _ a _) = a

isChild :: ATree Integer -> ATree Integer -> Bool   --I'm sorry it's unreadable again
--A leaf doesn't have children
_ `isChild` Lf x = False
--Base case is checking if Leaf is a child
(Lf child) `isChild` (Br (left) x (rigth)) = (child == (fromATree left)) || (child == (fromATree rigth))
--Induction case checks if either value in possible child tree equals one of the branches of parent tree
(Br (cleft) x (crigth)) `isChild` (Br (pleft) _ (prigth)) = 
    --for left we check if both left and right braches of child tree are children of the father's left branch
    (x == fromATree pleft && cleft `isChild` pleft && crigth `isChild` pleft) 
    --we have to check all the cases, even though it requires checking the whole tree
    || 
    --the same for the right branch
    (x == fromATree prigth && crigth `isChild` prigth && crigth `isChild` prigth) 
{-
t1 `isChild` t2
False

t2 `isChild` t1
True

t3 `isChild` t1
False

t1 `isChild` tree
True
-}