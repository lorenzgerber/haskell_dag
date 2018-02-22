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

data Weight a = Weight a deriving (Show, Eq, Ord)

weightInt :: Int -> Weight Int
weightInt a = Weight a

type Id = Int
type Origin = Int
type Destination = Int

addVertex :: Dag v e -> Weight v -> Dag v e 
addVertex (Dag a b) c = Dag (vertex:a) b
    where vertex = Vertex {vId = (length a), vWeight = c}  

addEdge :: Dag v e -> Origin -> Destination -> Weight e -> Dag v e
addEdge a b c d
    | (not.null) $ cycleDetect $ prepareList $ addEdge' a b c d = error $ "Cycle detected!"
    | otherwise = addEdge' a b c d  

addEdge' :: Dag v e -> Origin -> Destination -> Weight e ->  Dag v e
addEdge' (Dag a b) c d e = Dag a (edge:b)
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

makePrecede :: Dag v e -> [Int]
makePrecede a = reverse $ foldl makePrecede' [] $ prepareList a


makePrecede' :: [Int] -> ([Int],[Int]) -> [Int]
makePrecede' ts (x,xs)  = nub $ case elemIndex (head x) ts of
                                          Just i  -> uncurry(++) $ first(++xs) $ splitAt i ts
                                          _       -> ts ++ xs ++ x

visitVertex :: Destination -> Dag v e ->  Weight w -> Origin -> Weight w
visitVertex dest dag weight orig
    | (snd $ getDestinations (edges dag) orig) == [] = weight
      

--    | (snd $ getDestinations (edges dag) orig) /= [] = map (visitVertex dest dag weight) (snd $ getDestinations (edges dag) orig)
--    | orig == dest = weight


getWeightVertex :: Id -> Dag v e -> Weight v
getWeightVertex a b = vWeight $ head $ filter (\vertex -> vId vertex == a) (vertices b) 

getWeightEdge :: Origin -> Destination -> Dag v e -> Weight e
getWeightEdge a b c = eWeight $ head $ filter (\edge -> origin edge == a && destination edge == b) (edges c)

plus :: (Num a) => Weight a -> Weight a -> Weight a
(Weight a) `plus` (Weight b) = Weight (a + b)
 


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
