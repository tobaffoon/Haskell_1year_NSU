import Data.Array

arI :: Array (Integer,Integer) Integer
arI = array ((1,1),(4,4)) [((1,1), 10), ((1,2), 3), ((1,3), 1), ((1,4), 5), ((2,1), 2), ((2,2), 4), ((2,3), 9), ((2,4), 6), ((3,1), 20), ((3,2), 21), ((3,3), 0), ((3,4), 7), ((4,1), 19), ((4,2), 15), ((4,3), 31), ((4,4), 8)]
arCoord :: Array Integer (Double, Double)
arCoord = listArray (1,5) [((-5.23),4.2),(2.456,13.234),(11241,211.2),((-1),0),(8.3,3.8)]

arCoordS :: Array Integer (Double, Double)
arCoordS = listArray (1,4) [((-3), 4), (0,0), (3,(-4)), (12,8)]

sar :: Array Integer Double
sar = listArray (1,10) [1.3,1.2,1.9,1.25,1.4,0.1,1.2,1.32,1.23,1.5]

sar1 :: Array Integer Double
sar1 = listArray (1,10) [(-1.31), 0.099, 14.33, (-15.14), 98.68, (-0.998), 23.01, 5.666, 30.1235, (-30.1234)]

sum' arr = sum (elems arr)
sum1 arr = foldl1 (+) (elems arr)
sum2 arr = foldl1 (+) [arr ! a | a <- indices arr]

eq_idx :: Array (Integer, Integer) Integer -> [Integer]
eq_idx arr = [arr ! a | a <- indices arr, (fst a + snd a)==(arr ! a)]

sev_sev_eq_idx :: Array (Integer, Integer) Integer -> Array (Integer, Integer) Integer
sev_sev_eq_idx arr = arr // [(a, 77) | a <- indices arr, (fst a + snd a)==(arr ! a)]

poly_lenght :: Floating a => Array Integer (a,a) -> a
poly_lenght arr = sum [sqrt $ (fst (arr ! a) - fst (arr ! (a+1))) ** 2 + (snd (arr ! a) - snd (arr ! (a+1))) ** 2 | a <- init $ indices arr]

averaged_ar :: Floating a => Array Integer a -> Array Integer a
averaged_ar arr = listArray (l, r) new_ls where
    l = head $ indices arr
    l1 = head (drop 1 $ indices arr)
    r = last $ indices arr
    r1 = last (init $ indices arr)
    new_ls = [(arr ! l + arr ! l1) / 2] ++ [(arr ! (a-1) + arr ! a + arr ! (a+1)) / 3 | a <- drop 1 (init $ indices arr)] ++ [(arr ! r1 + arr ! r) / 2]
{-
sum1 (array ((0,1),(9,10)) [((i,j), i*i + j*j) | i <- [0..9], j <- [1..10]])
285

sum2 arI
161

sum' (fmap fst arCoord)
11245.526

eq_idx arI
[3,5,4,6,7,8]

sev_sev_eq_idx arI
array ((1,1),(4,4)) [((1,1),10),((1,2),77),((1,3),1),((1,4),77),((2,1),2),((2,2),77),((2,3),9),((2,4),77),((3,1),20),((3,2),21),((3,3),0),((3,4),77),((4,1),19),((4,2),15),((4,3),31),((4,4),77)]       

poly_lenght arCoordS
25.0

poly_lenght arCoord
22506.17872083843

averaged_ar sar
array (1,10) [(1,1.25),(2,1.4666666666666668),(3,1.45),(4,1.5166666666666666),(5,0.9166666666666666),(6,0.9),(7,0.8733333333333334),(8,1.25),(9,1.3499999999999999),(10,1.365)]

elems $ averaged_ar sar
[1.25,1.4666666666666668,1.45,1.5166666666666666,0.9166666666666666,0.9,0.8733333333333334,1.25,1.3499999999999999,1.365]

elems $ averaged_ar sar1
[-0.6055,4.373,-0.2370000000000001,32.623333333333335,27.514,40.23066666666667,9.226,19.599833333333333,1.8887000000000012,4.999999999988347e-5]
-}