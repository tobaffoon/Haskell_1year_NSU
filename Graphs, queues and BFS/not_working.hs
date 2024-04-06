import Data.Graph
--import Data.Tree
import Data.List (delete)
import Data.Array

data RTree a = NIL | RNode a [RTree a] deriving Show
data Queue a = Queue [a] [a]

newQueue :: Queue a
newQueue = Queue [] []

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty _             = False

push :: Queue a -> a -> Queue a
push (Queue front back) x = Queue front (x:back) -- x goes to the back of the line

pop :: Queue a -> (a, Queue a)
pop (Queue [] []) = error "No more elements left"

pop (Queue [] back) = pop (Queue (reverse back) [])

pop (Queue (x:front) back) = (x, Queue front back)

--добавить в дерево сыновей, занести их в очередь. Сыновья будут во втором списке Queue, остальное во втором списке. Шаг рекурсии когда второй список пустой

orGraph = buildG (1, 6) [(1, 2),(1, 4),(2, 5),(3, 5),(3, 6),(4, 2),(5, 4),(6, 6)]

data BFSColor = WHITE | GRAY | BLACK deriving (Show, Eq)
data BFSVertex = BFSVertex {vert :: Vertex, c :: BFSColor, p :: Maybe Vertex} deriving (Show, Eq)
type BFSGraph = (Graph, [BFSVertex])

--descendants :: Graph -> Vertex -> [Vertex]
--descendants gr v = fmap snd $ filter (\(parent, child) -> if parent == v then True else False) (edges gr)

--descendants_BFS :: Graph -> Vertex -> [BFSVertex]
--descendants_BFS gr src = fmap ((\vert -> BFSVertex {vert = vert, c = WHITE, p = Just $ src}) . snd) $ filter (\(parent, child) -> if parent == src then True else False) (edges gr)

descendants :: Graph -> Vertex -> [Vertex]
descendants gr v = gr ! v

white_descendants_BFS :: Graph -> [BFSVertex] -> Vertex -> [BFSVertex]
white_descendants_BFS gr bverts src = filter (\bv -> ((c bv) == WHITE) && ((vert bv) `elem` sons)) bverts
	where sons = descendants gr src

--this function returns pair of white vertices AND initial list where they are painted gray
--											|				|
--											v				v
paintGray_BFS :: [BFSVertex] -> [BFSVertex] -> [BFSVertex]
paintGray_BFS bvs whites = fmap (paintGray' whites) bvs where
	paintGray' :: [BFSVertex] -> BFSVertex -> BFSVertex
	paintGray' wht bfsv = if (bfsv `elem` wht)
							then BFSVertex {vert = (vert bfsv), c = GRAY, p = (p bfsv)}
							else bfsv

	{-paintGray' whites (v:nvs) = do
		let (res_whites, res_vs) = paintGray' whites nvs
		if (c v) == WHITE
			then ((v:res_whites), new_gray:res_vs)
			else (res_whites, res_vs)
				where new_gray = BFSVertex {vert = (vert v), c = GRAY, p = (p v)}-}

init_BFS :: Graph -> Vertex -> [BFSVertex]
{-init_BFS gr src = array (bounds gr) $ fmap turnBFS (assocs gr) where 
		turnBFS (idx, v)	if v == src 
									then BFSVertex {vert = v, c = GRAY, p = Nothing}
									else BFSVertex {vert = v, c = WHITE, p = Just $ v}
-}
init_BFS gr src = fmap turnBFS (indices gr)
	where turnBFS v =	if v == src 
								then BFSVertex {vert = v, c = GRAY, p = Nothing}
								else BFSVertex {vert = v, c = WHITE, p = Nothing}
			

bfs' :: BFSGraph -> [BFSVertex] -> [[BFSVertex]]
bfs' (info, bvs) level = if null level
							then [[]]
							else
								let (new_lvl, (ninfo, new_bvs)) = foldl (\accum y -> add_desc accum (vert y)) ([], (info, bvs)) level
								in ([level] ++ [new_lvl] ++ (bfs' (info, new_bvs) new_lvl))
						
add_desc :: ([BFSVertex], BFSGraph) -> Vertex -> ([BFSVertex], BFSGraph)
add_desc (new_lvl, (relations, verts)) x = do
							let whites = white_descendants_BFS relations verts x
							let	new_verts = paintGray_BFS verts $ whites
							(new_lvl ++ whites, (relations, new_verts))

{-bfs :: Graph -> Vertex -> RTree Vertex
bfs gr idx = snd $ bfs' (init_BFS gr idx) (newQueue `push` src) NIL where

src = BFSVertex {vert = idx, c = GRAY, p = Nothing}
bfs' :: BFSGraph -> Queue BFSVertex -> RTree Vertex -> (Queue BFSVertex, RTree Vertex)
bfs' bgr q tree = if empty q
					 then (q, NIL)
					 else do
						let (y, nq) = pop q
						let (desc_y, new_grays) = paintGray_BFS $ descendants_BFS bgr y
						let new_bgr = bgr // [(y, new_grays)]
						let newq = foldl (\nqq v -> nqq `push` v) nq desc_y
						(newq, RNode y (bfs_recurs new_bgr newq))
			

rtree_add :: RTree 
rtree_add tree v

bfs_recurs :: BFSGraph -> Queue Vertex -> [RTree Vertex]
bfs_recurs bgr q = 	do
					if empty q
						then []
						else do/
								let (nq, res) = bfs' bgr q
								(res : bfs_recurs bgr nq)
-}
--bfs :: Graph -> Vertex -> RTree Vertex
--bfs gr idx = restoreTree $ bfs' (init_BFS gr idx) [src] where

--src = BFSVertex {vert = idx, c = GRAY, p = Nothing}