data ATree a = ALeaf a | ABranch (ATree a) a (ATree a)
    deriving (Show, Read)

instance Eq a => Eq (ATree a) where
    a == b = areSimilar a b

myTree = ABranch(
            ABranch(
                ALeaf 21
            )
            2
            (
                ABranch(
                    ABranch(
                        ALeaf 62
                    )
                    43
                    (
                        ALeaf 66
                    )
                )
                22
                (
                    ALeaf 46
                )
            )
        )
        1
        (
            ABranch(
                ALeaf 34
            )
            3
            (
                ALeaf 35
            )
        )

myTree1 = ABranch(
            ABranch(
                ALeaf 21
            )
            2
            (
                ABranch(
                    ALeaf 46
                )
                22
                (
                    ABranch(
                        ALeaf 62
                    )
                    43
                    (
                        ALeaf 66
                    )
                )
            )
        )
        1
        (
            ABranch(
                ALeaf 35
            )
            3
            (
                ALeaf 34
            )
        )

myTree_WithChangedLeaf = ABranch(
            ABranch(
                ALeaf 21
            )
            2
            (
                ABranch(
                    ALeaf 46
                )
                22
                (
                    ABranch(
                        ALeaf 62
                    )
                    43
                    (
                        ALeaf 67        --this leaf is changed
                    )
                )
            )
        )
        1
        (
            ABranch(
                ALeaf 35
            )
            3
            (
                ALeaf 34
            )
        )

fringe :: ATree a -> [a]
fringe (ALeaf x) = [x]
fringe (ABranch left top right) = fringe left ++ [top] ++ fringe right

leafsum :: (Num a) => ATree a -> a
leafsum (ALeaf x) = x
leafsum (ABranch left top right) = leafsum left + top + leafsum right

even_apex :: (Integral a) => ATree a -> [a]
even_apex (ALeaf x) | x `mod` 2 == 0 = [x]
                    | otherwise = []
even_apex (ABranch left top right) | top `mod` 2 == 0 = even_apex left ++ [top] ++ even_apex right
                                   | otherwise = even_apex left ++ even_apex right

sister_apex :: ATree a -> [(a,a)]
sister_apex (ALeaf x) = []
sister_apex (ABranch left top right) = sister_apex left ++ [(fromALeaf left, fromALeaf right)] ++ sister_apex right

fromALeaf :: ATree a -> a
fromALeaf (ALeaf a) = a
fromALeaf (ABranch _ a _) = a

subtrees :: ATree a -> [ATree a]
subtrees (ALeaf a) = [ALeaf a]
subtrees (ABranch left top right) = subtrees left ++ subtrees right ++ [ABranch left top right]

areSimilar :: Eq a => ATree a -> ATree a -> Bool
areSimilar (ALeaf a) (ALeaf b) = a==b
areSimilar (ALeaf _) (ABranch _ _ _) = False
areSimilar (ABranch _ _ _) (ALeaf _) = False
areSimilar (ABranch left1 a right1) (ABranch left2 b right2) = (a==b) 
                                                                && 
                                                                ((areSimilar left1 left2 && areSimilar right1 right2)           
                                                                || 
                                                                (areSimilar left1 right2 && areSimilar right1 left2))

{-
    leafsum myTree
    335

    sister_apex myTree
    [(21,22),(62,66),(43,46),(2,3),(34,35)]

    subtrees myTree
    [ALeaf 21,ALeaf 62,ALeaf 66,ABranch (ALeaf 62) 43 (ALeaf 66),ALeaf 46,ABranch (ABranch (ALeaf 62) 43 (ALeaf 66)) 22 (ALeaf 46),ABranch (ALeaf 21) 2 (ABranch (ABranch (ALeaf 62) 43 (ALeaf 66)) 22 (ALeaf 46)),ALeaf 34,ALeaf 35,ABranch (ALeaf 34) 3 (ALeaf 35),ABranch (ABranch (ALeaf 21) 2 (ABranch (ABranch (ALeaf 62) 43 (ALeaf 66)) 22 (ALeaf 46))) 1 (ABranch (ALeaf 34) 3 (ALeaf 35))]

    areSimilar myTree myTree1
    True

    areSimilar myTree myTree_WithChangedLeaf 
    False

    areSimilar myTree1 myTree_WithChangedLeaf 
    False
-}