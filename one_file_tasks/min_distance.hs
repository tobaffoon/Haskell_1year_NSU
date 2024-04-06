findDistance :: Double -> (Double, Double) -> [(Double, Double)] -> Double
findDistance (-1) (x1, y1) (x:xs) =                                 --Fisrt iteration results on calculation of distance between first two points
    let x2 = fst x
        y2 = snd x
        d = (x1-x2)**2+(y1-y2)**2
        in findDistance (findDistance d (x2, y2) xs) (x1, y1) xs
findDistance acc (x1, y1) [x] =                                  --Last iteration receives only two points
    let x2 = fst x
        y2 = snd x
        d = (x1-x2)**2+(y1-y2)**2 in
        if acc > d 
            then d
            else acc
findDistance acc (x1, y1) [] = acc                                  --If there are only two points
findDistance acc (x1, y1) (x:xs) =                                  --General iteration
    let x2 = fst x
        y2 = snd x
        d = (x1-x2)**2+(y1-y2)**2 in
        if acc > d 
            then findDistance (findDistance d (x2, y2) xs) (x1, y1) xs          --Compares distances between (current and next) and (next and point after next)
            else findDistance (findDistance acc (x2, y2) xs) (x1, y1) xs

minDist (x:xs) = sqrt(findDistance (-1) x xs)                       --We calculated only the sum of differences of points coordinates, so we get their sqrt
{-
        minDist  [((-5.23),4.2),(2.456,13.234),(11241,211.2),((-1),0)]
        5.960947911196675
    minDist [(1.41, -13.1),(3.0,-0.01),(13.9,1.1),(15.54,11.99),(8.01,9.71)]
    7.867610819047926
        minDist [(0,1),(1,2),(-1,-1),(11.001,-1)]
        1.4142135623730951 = sqrt(2)
    minDist [(0,0),(4,7),(-1,-10),(6,8)]
    2.23606797749979 = sqrt(5)
-}