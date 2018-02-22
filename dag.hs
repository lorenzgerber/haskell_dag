import Data.List
import Data.Maybe
import Control.Arrow

data Vertex w = Vertex {vId :: Id
                       , vWeight :: Weight w
                       } deriving (Show)

data Edge w = Edge { eWeight :: Weight w
                   , origin :: Origin
                   , destination :: Destination
                   } deriving (Show)

data Dag v e = Dag { vertices :: [ Vertex v ]
               , edges :: [ Edge e ]
               } deriving (Show)   

data Weight a = Weight a deriving (Show, Eq)

weightInt :: Int -> Weight Int
weightInt a = Weight a

type Id = Int
type Origin = Int
type Destination = Int

addVertex :: Dag v e -> Weight v -> Dag v e 
addVertex (Dag a b) c = Dag (vertex:a) b
    where vertex = Vertex {vId = (length a), vWeight = c}  

addEdge :: Dag v e -> Origin -> Destination -> Weight e ->  Dag v e
addEdge (Dag a b) c d e = Dag a (edge:b)
    where edge = Edge {origin = c, destination = d, eWeight = e}


getDestinations :: [Edge w] -> Int -> ([Int], [Int])
getDestinations edges chosenOrigin = ([chosenOrigin], map (destination) 
                                     $ filter (\edge -> origin edge == chosenOrigin) edges)

getOrigins :: [Edge w] -> [Int]
getOrigins edges = foldl (\acc x -> (origin x):acc ) [] edges 


prepareList :: Dag v e -> [([Int],[Int])]
prepareList dag = map (getDestinations (edges dag)) (getOrigins (edges dag)) 

cycleDetect :: [([Int],[Int])] -> [[Int]]
cycleDetect xs = filter ((>1).length)
                 $ map (\[(a,as), (b,bs)] -> (a `intersect` bs) ++ (b `intersect`as))
                 $ combs 2 xs

combs 0 _ = [[]]
combs _ [] = []
combs k (x:xs) = map (x:) (combs (k-1) xs) ++ combs k xs

makePrecede :: [([Int],[Int])] -> [Int]
makePrecede a = foldl makePrecede' [] a


makePrecede' :: [Int] -> ([Int],[Int]) -> [Int]
makePrecede' ts (x,xs)  = nub $ case elemIndex (head x) ts of
                                          Just i  -> uncurry(++) $ first(++xs) $ splitAt i ts
                                          _       -> ts ++ xs ++ x




-- example data
a = addVertex (Dag [][]) (Weight 10)
b = addVertex a (Weight 10)
c = addVertex b (Weight 10)
d = addEdge c 0 1 (Weight 10)
e = addEdge d 0 2 (Weight 10)
f = addEdge e 1 2 (Weight 10)
g = addEdge f 2 1 (Weight 10)


