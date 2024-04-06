import Data.Graph
import Data.List (find)
import Data.Array

data RTree a = NIL | RNode a [RTree a] deriving Show
data Queue a = Queue [a] [a] deriving Show
data BFSColor = WHITE | GRAY | BLACK deriving (Show, Eq)
data BFSVertex = BFSVertex {vert :: Vertex, c :: BFSColor, p :: Maybe Vertex} deriving Show
type BFSGraph = (Graph, [BFSVertex])	--Graph is info about the edges, [BFSVertex] is info about each individual vertex

instance Eq BFSVertex where
	(==) x y = ((vert x) == (vert y))

newQueue :: Queue a
newQueue = Queue [] []

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty _             = False

push :: Queue a -> a -> Queue a
push (Queue front back) x = Queue front (x:back) -- x goes to the back of the line

refill :: Queue a -> Queue a
refill (Queue front back) = Queue (front ++ (reverse back)) []

pop :: Queue a -> (a, Queue a)
pop (Queue [] []) = error "No more elements left"
pop (Queue [] back) = pop $ refill (Queue [] back)
pop (Queue (x:front) back) = (x, Queue front back)

--returns all the queue contents
popAll :: Queue a -> ([a], Queue a)
popAll (Queue front back) = (popAll' (Queue front back), newQueue)	where
	popAll' :: Queue a -> [a]
	popAll' (Queue [] []) = []
	popAll' (Queue f b) = 
		let (x, nq) = pop (Queue f b)
		in (x : (popAll' nq))

emptyGr = buildG (1,100) []
orGraph = buildG (1, 6) [(1, 2),(1, 4),(2, 5),(3, 5),(3, 6),(4, 2),(5, 4),(6, 6)]
orGraph1 = buildG (1, 6) [(2, 1),(1, 4),(2, 5),(3, 5),(3, 6),(4, 2),(5, 4),(6, 6)]	--initial graph, but now you can get from 3 to 1
orGraph2 = buildG (1, 9) [(1,2),(1,4),(1,7),(2,3),(2,6),(2,5),(3,7),(4,1),(4,5),(4,8),(5,9),(7,2),(8,9),(9,8)]

--returns all the sons a vertex
descendants :: Graph -> Vertex -> [Vertex]
descendants gr v = gr ! v

--returns all the WHITE sons of the vertex according to given edges in the Graph 
white_descendants_BFS :: Graph -> [BFSVertex] -> Vertex -> [BFSVertex]
white_descendants_BFS gr bverts src =	do
									let sons = descendants gr src
									let wdescs = filter (\bv -> ((c bv) == WHITE) && ((vert bv) `elem` sons)) bverts
									fmap (makeGray . set_parent src) wdescs
									
--set parent of a vertex to some other vertex
set_parent :: Vertex -> BFSVertex -> BFSVertex
set_parent v bv = BFSVertex {vert = (vert bv), c = (c bv), p = Just $ v}

--this function returns list of vertices, where some of them are now painted gray
paintGray_BFS :: [BFSVertex] -> [BFSVertex] -> [BFSVertex]
paintGray_BFS bvs whites = fmap (paintGray' whites) bvs where
	paintGray' :: [BFSVertex] -> BFSVertex -> BFSVertex
	paintGray' wht bfsv = let new_bfsv = find (==bfsv) wht			--checks if vertex is in the 'whites' list 
							in maybePaint bfsv new_bfsv
							
--paints vertex gray and gives it the parent, if the vertex is in the white list (it is a white descendant of the parent)
maybePaint :: BFSVertex -> Maybe BFSVertex -> BFSVertex		
maybePaint _ (Just bv) = bv
maybePaint bv Nothing = bv										--if it is not a descendant, don't do anything

--makes Vertex gray
makeGray :: BFSVertex -> BFSVertex
makeGray BFSVertex {vert = x, c = _, p = y} = BFSVertex {vert = x, c = GRAY, p = y} 


--creates list of initial vertices but with colors and precesstors
init_BFS :: Graph -> Vertex -> [BFSVertex]
init_BFS gr src = fmap turnBFS (indices gr)
	where turnBFS v =	if v == src 
								then BFSVertex {vert = v, c = GRAY, p = Nothing}
								else BFSVertex {vert = v, c = WHITE, p = Nothing}
			
--returns list of vertices on all the levels of tree
bfs' :: BFSGraph -> Queue BFSVertex -> [[BFSVertex]]
--info is Graph containing information about edges
--bvs is list of vertices with colors and parents
bfs' (info, bvs) q = if empty q
							then []
							else do
								let (level, nq) = popAll q									--pops all the sons of the previous layer (basically while q is not empty -> pop)
								let (next_q, (_, new_bvs)) = foldl (\accum y -> add_desc accum y) (nq, (info, bvs)) level	--for every son on the current layer get new queue (push sons) and new list of vertices (make sons gray)
								([level] ++ (bfs' (info, new_bvs) next_q))
								
--gets white descendants, paints them gray, puts them in the queue, returns new queue and list of vertices
add_desc :: (Queue BFSVertex, BFSGraph) -> BFSVertex -> (Queue BFSVertex, BFSGraph)
add_desc (q, (relations, verts)) x = do
							let whites = white_descendants_BFS relations verts (vert x)		--get white descendants
							let	new_verts = paintGray_BFS verts $ whites					--changes the list of vertices so that they are gray now and have a parent
							let nq = foldl (\queue x -> queue `push` x) q whites			--pushes all the descendants in the queue
							(nq, (relations, new_verts))

--returns spanning tree of the graph using BFS starting from the vertex with number "idx"
bfs :: Graph -> Vertex -> RTree Vertex
bfs gr idx = restoreTree $ bfs' (gr, init_BFS gr idx) (newQueue `push` src) where
	src = BFSVertex {vert = idx, c = GRAY, p = Nothing}
	
--returns a Tree based on a list of levels (every vertex has a parent, so we can determine who to connect)
restoreTree :: [[BFSVertex]] -> RTree Vertex
restoreTree [] = error "Impøassible graph!"
restoreTree (x:xs) = head (restoreTree' x xs) where
	--returns a List of given vertices with their sons attached
	restoreTree' :: [BFSVertex] -> [[BFSVertex]] -> [RTree Vertex]
	restoreTree' y ys = fmap (restoreTree'' ys) y where
		--returns a Node with its sons attached
		restoreTree'' :: [[BFSVertex]] -> BFSVertex -> RTree Vertex
		restoreTree'' [] par = RNode (vert par) []			--Leaf case
		restoreTree'' ls par = 
				let sons = filter (\v -> v `isChild` (vert par)) (head ls)	--take head == only vertices on THIS level
				in RNode (vert par) (restoreTree' sons (tail ls))			--add sons' sons using the next level of the tree
				
isChild :: BFSVertex -> Vertex -> Bool
a `isChild` b = (p a) `vert_eq` b where
	Nothing `vert_eq` _ = False
	(Just x) `vert_eq` y = (x == y)
	
{-
bfs emptyGr 1
RNode 1 []

bfs orGraph 3
RNode 3 [RNode 5 [RNode 4 [RNode 2 []]],RNode 6 []]

bfs orGraph 1
RNode 1 [RNode 2 [RNode 5 []],RNode 4 []]

bfs orGraph1 3
RNode 3 [RNode 5 [RNode 4 [RNode 2 [RNode 1 []]]],RNode 6 []]

bfs orGraph2 2
RNode 2 [RNode 3 [RNode 7 []],RNode 5 [RNode 9 [RNode 8 []]],RNode 6 []]
-}