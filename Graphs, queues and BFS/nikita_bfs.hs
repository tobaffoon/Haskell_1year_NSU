import Data.Graph
import Data.Array
import Data.Tree
-- 1 задание --
a :: Graph
a = buildG (1,6) [(1,2),(1,4),(2,5),(4,2),(5,4),(3,5),(3,6),(6,6)]

emptyGr = buildG (1,100) []
orGraph = buildG (1, 6) [(1, 2),(1, 4),(2, 5),(3, 5),(3, 6),(4, 2),(5, 4),(6, 6)]
orGraph1 = buildG (1, 6) [(2, 1),(1, 4),(2, 5),(3, 5),(3, 6),(4, 2),(5, 4),(6, 6)]	--initial graph, but now you can get from 3 to 1
orGraph2 = buildG (1, 9) [(1,2),(1,4),(1,7),(2,3),(2,6),(2,5),(3,7),(4,1),(4,5),(4,8),(5,9),(7,2),(8,9),(9,8)]

-- 2 задание -- 

showTree' :: Tree Int -> Tree String                          --}
showTree' (Node a sons) = Node (show a) (map showTree' sons)  --}
showTree :: Tree Int -> String                                --} удобный вывод дерева на экран через drawTree из Data.Tree
showTree a = drawTree (showTree' a)                           --}

addNode :: Tree Int -> Int -> Int -> Tree Int                                                                           --}
addNode (Node a sons) pred son | a == pred = Node a (sons++[Node son []])                                               --} добавляет в дерево вершину son под нужного предка pred
                                | otherwise    = Node a (map addNode' sons) where addNode' tree = addNode tree pred son --}
updateQueue :: [Int] -> Graph  -> Int -> [Int]     --}
updateQueue queue graph elem = queue ++ graph!elem --} добавляет в очередь все вершины, смежные с elem
updateGraph :: Graph -> Int -> Graph                                                    --} удаляет из списка смежности графа уже исследованные вершины:
updateGraph graph elem = listArray (bounds graph) (map (filter (/=elem)) (elems graph)) --} updateGraph (array (1,3) [(1,[2,3]), (2,[1,3]), (3,[1,2])]) 2 -> array (1,3) [(1,[3]), (2,[1,3]), (3,[1])]
bfs' :: Graph -> Tree Int -> [Int] -> Int -> Tree Int
bfs' graph tree queue curNode| null queue = tree
                             | not (null (graph!curNode)) = let adjNode = head(graph!curNode) in bfs' (updateGraph graph adjNode) (addNode tree curNode adjNode) (updateQueue queue (updateGraph graph adjNode) adjNode) curNode
                             | otherwise                  = bfs' graph tree (tail queue) (head queue)
-- С каждым шагом рекурсии обновляется граф (удаление обрабатываемого потомка из списка смежностей), обрабатываемая вершина добавляется в дерево под вершу curNode, в очередь добавляются все вершины, смежный с обрабатываемым потомком.
-- Когда у curNode кончаются потомки, в curNode помещается первая вершина из queue
bfs graph start = bfs' (updateGraph graph start)  (Node start []) (graph!start) start

-- 3 задание --
{-
showTree (bfs a 3)
Результат:
3
|
+- 6
|
`- 5
   |
   `- 4
      |
      `- 2
-}
