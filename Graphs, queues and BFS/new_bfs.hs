import Data.Graph
import Data.List (find)
import Data.Array

data RTree a = NIL | RNode a [RTree a] deriving Show

instance Eq BFSVertex where
	(==) x y = ((vert x) == (vert y))

emptyGr = buildG (1,100) []
orGraph = buildG (1, 6) [(1, 2),(1, 4),(2, 5),(3, 5),(3, 6),(4, 2),(5, 4),(6, 6)]
orGraph1 = buildG (1, 6) [(2, 1),(1, 4),(2, 5),(3, 5),(3, 6),(4, 2),(5, 4),(6, 6)]	--initial graph, but now you can get from 3 to 1
orGraph2 = buildG (1, 9) [(1,2),(1,4),(1,7),(2,3),(2,6),(2,5),(3,7),(4,1),(4,5),(4,8),(5,9),(7,2),(8,9),(9,8)]

paintGray :: Graph -> Vertex -> Graph
paintGray graph father = listArray (bounds graph) (fmap (/=father) (elems graph))

addDescendants :: Graph -> [Vertex] -> Vertex -> [Vertex]
addDescendants graph queue father = queue ++ (graph ! father)

hangNode :: Tree Vertex -> Vertex -> Vertex -> Tree Vertex
hangNode (Node root children) newNode parent 	| root == parent = Node root (new : children)	where new = Node newNode []
												| otherwise 

--returns list of vertices on all the levels of tree
bfs' :: Graph -> Tree Vertex -> [Vertex] -> Int -> Tree Vertex
--info is Graph containing information about edges
--bfs is list of vertices with colors and parents
bfs' graph tree queue gray 	| empty queue = tree
							| otherwise 
								
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