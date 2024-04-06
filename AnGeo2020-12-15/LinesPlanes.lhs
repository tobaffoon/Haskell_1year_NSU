{-# UnicodeSyntax #-}

\section{Прямые и плоскости в 3-мерном пространстве}

\begin{code}

module LinesPlanes where

import AnGeo
import Lines
-- import Data.Semigroup
-- import Data.Monoid

\end{code}


Зададим тип данных "плоскость", задаваемый скалярным произведением:
$$
(\vec{r}_0 - \vec{r}) \cdot \vec{n} = 0,
$$
т.е. описываем точки плоскости в которые приходит радиус-вектор $\vec{r}$ с помощью радиус-вектора начальной точки $\vec{r}_0$ и нормали $\vec{n}$.

\begin{code}
data Plane = Pl {mo, normal :: Vec} deriving (Read)
\end{code}

Зададим тип данных "каноническое уравнение плоскости" в соответствии с каноническим уравнением плоскости:
$$
Ax + By + Cz + D = 0.
$$

\begin{code}
data CPlane = CPl {aa,bb,cc,dd :: Double} deriving (Read)
\end{code}

Зададим функцию нахождения нормали для плоскости, заданной в канонической форме

\begin{code}
normalForCPlane :: CPlane -> Vec
normalForCPlane (CPl a b c _) = Vc a b c
-- normalForCPlane (CPl a b c d) = Vc a b c
\end{code}

Зададим функции-конструкторы плоскости:

\begin{code}
planeFromPointAndVec :: Point -> Vec -> Plane
planeFromPointAndVec p u = Pl (fromPoint p) u
\end{code}

(неплохо бы обдумать вырожденные случаи)

\begin{code}

det2 :: Double -> Double -> Double -> Double -> Double
det2 a b c d = a * d - b * c

planeFrom3Points :: Point -> Point -> Point -> Plane
planeFrom3Points (Pt x1 y1 z1) (Pt x2 y2 z2) (Pt x3 y3 z3) = 
	if aa cpl == 0 && bb cpl == 0 && cc cpl == 0
		then error "The case is degenerate"
		else cplaneToPlane cpl where
			d1 = det2 (y2 - y1) (z2 - z1) (y3 - y1) (z3 - z1)
			d2 = - det2 (x2 - x1) (z2 - z1) (x3 - x1) (z3 - z1)
			d3 = det2 (x2 - x1) (y2 - y1) (x3 - x1) (y3 - y1)
			cpl = CPl d1 d2 d3 (-(x1*d1 + y1*d2 + z1*d3))
\end{code}

Случай NaN - случай параллельных прямых
\begin{code}
planeFrom2Lines :: Line -> Line -> Plane
planeFrom2Lines l1 l2 = if ((isNaN (skewLinesDistance l1 l2)) && l1 /= l2) 
							|| 
							skewLinesDistance l1 l2 == 0
	then if ro l1 == ro l2
			then planeFrom3Points 
					(toPoint $ ro l1) 
					(toPoint $ pls (ro l1) (dir l1)) 
					(toPoint $ pls (ro l2) (dir l2))
			else if (toPoint $ pls (ro l1) (dir l1)) == (toPoint $ ro l2)
				then planeFrom3Points 
						(toPoint $ ro l1) 
						(toPoint $ ro l2) 
						(toPoint $ pls (ro l2) (dir l2))
				else planeFrom3Points 
						(toPoint $ ro l1) 
						(toPoint $ ro l2) 
						(toPoint $ pls (ro l1) (dir l1))
	else error "The lines are skew"
\end{code}

Преобразование типов плоскостей:

\begin{code}
planeToCPlane :: Plane -> CPlane
planeToCPlane (Pl base norm) = CPl a b c d where
	a = vx norm
	b = vy norm
	c = vz norm
	d = - a * (vx base) - b * (vy base) - c * (vz base)
\end{code}

\begin{code}
cplaneToPlane :: CPlane -> Plane
cplaneToPlane (CPl a b c d) = Pl m n where
	m  	| a /= 0 = Vc ((- d)/ a) 0 0
		| b /= 0 = Vc 0 ((- d) / b) 0
		| c /= 0 = Vc 0 0 ((- d) / c)	
	n = Vc a b c
\end{code}

Красивое отображение канонической плоскости в виде уравнения:

\begin{code}
instance Show CPlane where
  show cplane = "(" ++ (show $ aa cplane) 
	++ ")*x + (" ++ (show $ bb cplane) ++ ")*y + (" 
	++ (show $ cc cplane) ++ ")*z + (" ++ (show $ dd cplane) ++ ")"

instance Show Plane where
  show plane = show (mo plane) ++ " + t*" ++ show (normal plane)
\end{code}

Проверка принадлежености точки плоскости (в обеих формах)

\begin{code}
pointOnPlane :: Point -> Plane -> Bool
pointOnPlane pt plane = pointOnCPlane pt (planeToCPlane plane)

pointOnCPlane :: Point -> CPlane -> Bool
pointOnCPlane (Pt x y z) (CPl a b c d) = a*x + b*y + c*z + d == 0
\end{code}

Проверка принадлежености прямой плоскости

Если две несовпадающие точки прямой принадлежат плоскости, то и прямая принадлежит ей
\begin{code}
lineOnPlane :: Line -> Plane -> Bool
lineOnPlane (Ln beg dir) plane = 	(pointOnPlane (toPoint beg) plane) 
									&& 
									(pointOnPlane (toPoint $ pls beg dir) plane)

lineOnCPlane :: Line -> CPlane -> Bool
lineOnCPlane (Ln beg dir) cplane = 	(pointOnCPlane (toPoint beg) cplane) 
									&& (
									pointOnCPlane (toPoint $ pls beg dir) cplane)
\end{code}

Проверка совпадения двух плоскостей

\begin{code}
instance Eq Plane where
	u == w = ((normal u) `coll` (normal w))
			&&
			(pointOnPlane (toPoint $ mo u) w)
			&&
			(pointOnPlane (toPoint $ mo w) u)

instance Eq CPlane where
	u == w =( 
			((aa u) * (bb w))
			==
			((bb u) * (aa w))
			)
			&&
			( 
			((bb u) * (cc w))
			==
			((cc u) * (bb w))
			)
			&&
			( 
			((cc u) * (dd w))
			==
			((dd u) * (cc w))
			)
\end{code}

Проверка параллельности двух плоскостей

\begin{code}
planeParall :: Plane -> Plane -> Bool
planeParall p1 p2 = normal p1 `coll` normal p2

cplaneParall :: CPlane -> CPlane -> Bool
cplaneParall p1 p2 =(
					((aa p1) * (bb p2))
					==
					((bb p1) * (aa p2))
					)
					&&
					( 
					((bb p1) * (cc p2))
					==
					((cc p1) * (bb p2))
					)
\end{code}

Проверка перпедикулярности двух плоскостей

\begin{code}
planePerp :: Plane -> Plane -> Bool
planePerp p1 p2 = (normal p1) `perp` (normal p2)

cplanePerp :: CPlane -> CPlane -> Bool
cplanePerp p1 p2 = (Vc (aa p1) (bb p1) (cc p1)) `perp` (Vc (aa p2) (bb p2) (cc p2))
--То же самое, что и для Plane
\end{code}


Проверка параллельности прямой и плоскости

\begin{code}
lineAndPlaneParall :: Line -> Plane -> Bool
lineAndPlaneParall line plane = (dir line) ┴ (normal plane)
\end{code}

Проверка перпедикулярности прямой и плоскости

\begin{code}
lineAndPlanePerp :: Line -> Plane -> Bool
lineAndPlanePerp line plane = (dir line) `coll` (normal plane)
\end{code}

Нахождение угла между плоскостями (в градусах бы)...

\begin{code}
planeAngle :: Plane -> Plane  -> Double
planeAngle p1 p2 = lineAngle (Ln (mo p1) (normal p1)) (Ln (mo p2) (normal p2))
\end{code}

Нахождение угла между прямой и плоскостью (в градусах бы)...

\begin{code}
lineAndPlaneAngle :: Line -> Plane  -> Double
lineAndPlaneAngle l p = 90 + lineAngle l (Ln (mo p) (normal p))
\end{code}

Нахождение расстояния между точкой и плоскостью

\begin{code}
pointToPLaneDistance :: Point -> Plane -> Double
pointToPLaneDistance pt pl = if pointOnPlane pt pl 
								then 0
								else (norma $ fromOrSeg $ OrS pt (toPoint $ mo pl)) * (abs $ sin $ (lineAndPlaneAngle (lineFrom2Points pt (toPoint $ mo pl)) pl) * pi / 180)

pointToCPLaneDistance :: Point -> CPlane -> Double
pointToCPLaneDistance (Pt x y z) (CPl a b c d) = (abs (a*x + b*y + c*z +d)) / (sqrt (a^2 + b^2 + c^2))
\end{code}

Нахождение линии пересечения двух плоскостей, заданных уравнением...

Нерабочая идея про пересечение множеств линий равноудалённых от точек заданных в плоскастях
Эти множества можно спроектировать как окружности на плоскость перпендикулярную линии пересечения
Думал, что получившиеся окружности касаются, но в общем случае они пересекаются и в одной из точек пересечения - точка принадлежащая линии пересечения
Найти эти точки намного сложнее, чем найти точку касания, хотя проверка вроде несложная (pointOnPlane)
Должна быть другая идея

begin{code}
lineIntersectionOf2Planes :: Plane -> Plane -> Line
lineIntersectionOf2Planes p1 p2 = if planeParall p1 p2 
	then error "Cannot find the intersection line"
	else Ln base ((normal p1) `vprod` (normal p2)) where
		r1 = (pointToPLaneDistance (toPoint $ mo p1) p2) * (abs $ sin $ (planeAngle p1 p2) * pi / 180)
		r2 = (pointToPLaneDistance (toPoint $ mo p2) p1) * (abs $ sin $ (planeAngle p1 p2) * pi / 180)
		base_vec = pls (mo p1) ((r1 / (r1 + r2)) `kprod` (fromOrSeg $ OrS (toPoint $ mo p1) (toPoint $ mo p2)))
		base | r1 == 0 = mo p1
			 | r2 == 0 = mo p2
			 | otherwise = Vc (vx base_vec) (vy base_vec) (vz $ mo p1)
end{code}

\begin{code}
pointIntersectionOfLineAndPlane :: Line->Plane->Vec
pointIntersectionOfLineAndPlane l pl = Vc (vx(ro l)+(vx(dir l))*(t l pl)) (vy(ro l)+(vy(dir l))*(t l pl)) (vz(ro l)+(vz(dir l))*(t l pl))
    where t l pl = (-1)*(((normal pl) sprod (ro l)) + ((-1)*((mo pl) sprod (normal pl))))/((normal pl) sprod (dir l))

perpVec :: Vec->Vec->Vec
perpVec v1 v2 = Vc (x v1 v2) (vy v2) (vz v2)
    where x v1 v2 = (-1)*((vy v2)*(vy v1)+(vz v2)*(vz v1))/(vx v1)

lineIntersectionOf2Planes :: Plane -> Plane -> Line
lineIntersectionOf2Planes pl1 pl2 | (pl1 == pl2) = error "Planes are equal!"
                                  | (planeParall pl1 pl2) = error "Planes are parallel!"
                                  | otherwise = Ln (pointIntersectionOfLineAndPlane (Ln (mo pl1) (perpVec (normal pl1) (mo pl2))) pl2) ((normal pl1) vprod (normal pl2))
\end{code}