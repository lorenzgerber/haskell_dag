import Data.List
import Data.Maybe
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

weightInt :: Int -> Weight Int
weightInt a = Weight a

plus :: (Num a) => Weight a -> Weight a -> Weight a
(Weight a) `plus` (Weight b) = Weight (a + b)

type Id = Int
type Origin = Int
type Destination = Int

addVertex :: Dag w -> Weight w -> Dag w 
addVertex (Dag a b) c = Dag (vertex:a) b
    where vertex = Vertex {vId = (length a), vWeight = c}  

addEdge :: Dag w -> Origin -> Destination -> Weight w -> Dag w
addEdge a b c d
    | (not.null) $ cycleDetect $ prepareList $ addEdge' a b c d = error $ "Cycle detected!"
    | otherwise = addEdge' a b c d  

addEdge' :: Dag w -> Origin -> Destination -> Weight w ->  Dag w
addEdge' (Dag a b) c d e = Dag a (edge:b)
    where edge = Edge {origin = c, destination = d, eWeight = e}


getDestinations :: [Edge w] -> Int -> ([Int], [Int])
getDestinations edges chosenOrigin = ([chosenOrigin], map (destination) 
                                     $ filter (\edge -> origin edge == chosenOrigin) edges)

getOrigins :: [Edge w] -> [Int]
getOrigins edges = foldl (\acc x -> (origin x):acc ) [] edges 


prepareList :: Dag w -> [([Int],[Int])]
prepareList dag = map (getDestinations (edges dag)) (getOrigins (edges dag)) 

cycleDetect :: [([Int],[Int])] -> [[Int]]
cycleDetect xs = filter ((>1).length)
                 $ map (\[(a,as), (b,bs)] -> (a `intersect` bs) ++ (b `intersect`as))
                 $ combs 2 xs

combs 0 _ = [[]]
combs _ [] = []
combs k (x:xs) = map (x:) (combs (k-1) xs) ++ combs k xs

makePrecede :: Dag w -> [Int]
makePrecede a = reverse $ foldl makePrecede' [] $ prepareList a


makePrecede' :: [Int] -> ([Int],[Int]) -> [Int]
makePrecede' ts (x,xs)  = nub $ case elemIndex (head x) ts of
                                          Just i  -> uncurry(++) $ first(++xs) $ splitAt i ts
                                          _       -> ts ++ xs ++ x

--processer :: TL TL_index Edge_index
-- TL_lenght == TL_index + 1 && Edge_length == Edge_index + 1 = return TL
-- Edge_length == Edge_index = call processer with edge_index = 0, TL_index + 1
-- else call processer with weight applied and Edge_index + 1 


chopTripleList a b = case elemIndex a (makePrecede b) of
                      Just i -> drop i (makeTripleList b)
                      _      -> error "invalid Id"


makeTripleList :: Dag w -> [(Int, Weight w, [Edge w])]
makeTripleList a =  zip3  (makePrecede a) (map (getWeightVertex a) (makePrecede a)) (map (getOutBoundEdges a) (makePrecede a))

getOutBoundEdges :: Dag w -> Origin -> [Edge w]
getOutBoundEdges b a = filter (\edge -> origin edge == a) (edges b)

getWeightVertex :: Dag w -> Id -> Weight w
getWeightVertex b a = vWeight $ head $ filter (\vertex -> vId vertex == a) (vertices b) 

getWeightEdge :: Origin -> Destination -> Dag w -> Weight w
getWeightEdge a b c = eWeight $ head $ filter (\edge -> origin edge == a && destination edge == b) (edges c)


 


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
