import Data.List
import Data.Maybe
import Data.Ord
import Control.Arrow

data Vertex w = Vertex {vId :: Id
                       , vWeight :: Weight w
                       } deriving (Show, Eq, Ord)

data Edge w = Edge { eWeight :: Weight w
                   , origin :: Origin
                   , destination :: Destination
                   } deriving (Show, Eq, Ord)

data Dag w = Dag { vertices :: [ Vertex w ]
               , edges :: [ Edge w ]
               } deriving (Show)   

data Weight a = Weight a deriving (Show, Eq, Ord)

type Id = Int
type Origin = Int
type Destination = Int

plus :: (Num a) => Weight a -> Weight a -> Weight a
(Weight a) `plus` (Weight b) = Weight (a + b)

addVertex :: Dag w -> Weight w -> Dag w 
addVertex (Dag a b) c = Dag (vertex:a) b
    where vertex = Vertex {vId = (length a), vWeight = c}  

addEdge :: Dag w -> Origin -> Destination -> Weight w -> Dag w
addEdge a b c d
    | (not.null) $ cycleDetect $ addEdge' a b c d = error $ "Cycle detected!"
    | otherwise = addEdge' a b c d  

addEdge' :: Dag w -> Origin -> Destination -> Weight w ->  Dag w
addEdge' (Dag a b) c d e = Dag a (edge:b)
    where edge = Edge {origin = c, destination = d, eWeight = e}

cycleDetect :: Dag w -> [[Int]]
cycleDetect dag = filter ((>1).length)
                 $ map (\[(a,as), (b,bs)] -> (a `intersect` bs) ++ (b `intersect`as))
                 $ combs 2 $ map (getDestinations (edges dag)) (getOrigins (edges dag))

topoSort :: Dag w -> [Int]
topoSort dag = reverse $ foldl topoSort' [] $ map (getDestinations (edges dag)) (getOrigins (edges dag))

topoSort' :: [Int] -> ([Int],[Int]) -> [Int]
topoSort' ts (x,xs)  = nub $ case elemIndex (head x) ts of
                                          Just i  -> uncurry(++) $ first(++xs) $ splitAt i ts
                                          _       -> ts ++ xs ++ x

getDestinations :: [Edge w] -> Int -> ([Int], [Int])
getDestinations edges chosenOrigin = ([chosenOrigin], map (destination) 
                                     $ filter (\edge -> origin edge == chosenOrigin) edges)

getOrigins :: [Edge w] -> [Int]
getOrigins edges = foldl (\acc x -> (origin x):acc ) [] edges 

combs 0 _ = [[]]
combs _ [] = []
combs k (x:xs) = map (x:) (combs (k-1) xs) ++ combs k xs


pathCost :: (Num w) => Dag w -> [Int] -> Weight w
pathCost a b
    |length b == 0 = error "empty path"
    |length b == 1 = getWeightVertex a (b!!0)
    |length b == 2 = (getWeightVertex a (b!!0)) `plus` (getWeightVertex a (b!!1)) `plus` (getWeightEdge a (b!!0) (b!!1)) 
    |otherwise = pathCost' a (tail b) ((getWeightVertex a (b!!0)) `plus` (getWeightEdge a (b!!0) (b!!1)))

pathCost' :: (Num w) => Dag w -> [Int] -> Weight w -> Weight w
pathCost' a b c
    |length b == 0 = c
    |length b == 1 = getWeightVertex a (b!!0) `plus` c
    |otherwise     = pathCost' a (tail b) (getWeightVertex a (b!!0) `plus` getWeightEdge a (b!!0) (b!!1) `plus` c)

getOutBoundEdges :: Dag w -> Origin -> [Edge w]
getOutBoundEdges b a = filter (\edge -> origin edge == a) (edges b)

getWeightVertex :: Dag w -> Id -> Weight w
getWeightVertex b a = vWeight $ head $ filter (\vertex -> vId vertex == a) (vertices b) 

getWeightEdge :: Dag w -> Origin -> Destination -> Weight w
getWeightEdge dag orig dest = eWeight $ head $ filter (\edge -> origin edge == orig && destination edge == dest) (edges dag)


 


-- example data
a = addVertex (Dag [][]) (Weight 10)
b = addVertex a (Weight 10)
c = addVertex b (Weight 10)
d = addVertex c (Weight 10)
e = addVertex d (Weight 10)
f = addVertex e (Weight 10)
g = addVertex f (Weight 10)
h = addEdge g 5 6 (Weight 10)
i = addEdge h 6 1 (Weight 10)
j = addEdge i 0 1 (Weight 10)
k = addEdge j 0 2 (Weight 10)
l = addEdge k 1 3 (Weight 10)
m = addEdge l 2 4 (Weight 10)
n = addEdge m 2 3 (Weight 10)
o = addEdge n 3 4 (Weight 10)
