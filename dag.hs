-- |The DAG module provides functionality to compose directed
--acyclic graphs. 
--
--During assembly, the resulting DAG's are 
--checked for cycles. Weights have to be assigned for both 
--Vertices and Edges. The type for the weight can be chosen
--freely as long as Num is implemented. Additionally, 
--functionality to determine weight of the longest path 
--is provided.
module Dag
( Vertex ( Vertex),
  Edge ( Edge ),
  Dag ( Dag ),
  Weight ( Weight ),
  WeightVertex,
  WeightEdge,
  addVertex,
  addEdge,
  topoSort,
  longestPath,
  weightLongestPath,
  getWeightVertex,
  getWeightEdge
) where

import Data.List
import Data.Maybe
import Data.Ord
import Control.Arrow
import Data.Char

-- |Basic Type DAG
data Dag w = Dag { vertices :: [ Vertex w ]
               , edges :: [ Edge w ]
               } deriving (Show)   

-- |Basic Type Vertex to build a DAG
data Vertex w = Vertex {vId :: Id
                       , vWeight :: Weight w
                       } deriving (Show, Eq, Ord)

-- |Basic Type Edge to build a DAG
data Edge w = Edge { eWeight :: Weight w
                   , origin :: Origin
                   , destination :: Destination
                   } deriving (Show, Eq, Ord)

-- |Weight, to be assigned to Vertices and Edges
--Used as type parameter. Additional to Eq and Ord,
--Weights need to implement Num. 
data Weight w =  Weight w deriving (Show, Eq, Ord)

-- |Vertex and Edge Weight extractor functions.
--Each a function with these type signatures have to be
--provided for calculating the weight of the longest path
type WeightVertex w = (Dag w -> Id -> w)
type WeightEdge w = (Dag w -> Origin -> Destination -> w)

-- |Helper function to extract the actual
--Type Parameter from parameterized Weight w type
extractor :: Weight w -> w
extractor (Weight w) = w

-- |Type aliases for improved readability of the code
type Id = Int
type Origin = Int
type Destination = Int

-- |Type Class Plus for implementing polymorphism
--All types that shall be used as Weight type
--paramter shall implement Plus w Type Class.
class Plus w where 
    plus :: w -> w -> Weight w  

instance Plus Int where
    plus a b = Weight (a + b) 

instance Plus Integer where
    plus a b = Weight (a + b)

instance Plus Char where
    plus a b = Weight (chr ((ord a) + (ord b)))

instance (Plus w) => Plus [w] where
    plus [] xs = (Weight xs)
    plus xs [] = (Weight xs)
    plus xs ys = (Weight (xs ++ ys))

-- |Type Class Comp for implementing polymorphism
--All types that shall be used as Weight type
--parameter shall implement Comp w Type Class.
class (Ord w) => Comp w where
    comp :: w -> w -> Ordering

instance Comp Int where
    comp x y  
        | x == y    = EQ
        | x <= y    = LT
        | otherwise = GT

instance Comp Integer where
    comp x y
        | x == y    = EQ
        | x <= y    = LT
        | otherwise = GT

instance Comp Char where
    comp x y
        | x == y    = EQ
        | x >= y    = LT
        | otherwise = GT

instance (Comp w, Eq w, Ord w) => Comp [w] where
    comp x y
        | x == y    = EQ
        | x <= y    = GT
        | otherwise = LT

-- |Custom Comparing function to apply Comp w Type Class
--in maximumBy function
customComparing :: (Ord w, Comp w) => (b -> w) -> b -> b -> Ordering
customComparing a b c = comp (a b) (a c)  

-- |implementation of getWeightVertex
--function to extract weight from Vertices. Is used as function
--parameter for calculating path weights
getWeightVertex :: Dag w -> Id -> w
getWeightVertex b a = extractor $vWeight $ head $ filter (\vertex -> vId vertex == a) (vertices b) 

-- |implementation of getWeightEdge
--function to extract weight from Edges. Is used as function
--parameter for calculating path weights.
getWeightEdge :: Dag w -> Origin -> Destination -> w
getWeightEdge dag orig dest = extractor $ eWeight $ head $ filter (\edge -> origin edge == orig && destination edge == dest) (edges dag)

-- |addVertex adds a Vertex with weight to a dag
--The function creates an numeric id for the added
--vertex which is based on the number of vertices
--in the current dag (before addition).
addVertex :: Dag w -> Weight w -> Dag w 
addVertex (Dag a b) c = Dag (vertex:a) b
    where vertex = Vertex {vId = (length a), vWeight = c}  

-- |addEdge adds an edge from Origin to Destination with a
--specified weight to a dag.
--This function checks if on addition a cycle arises in 
--which case an error results.
addEdge :: Dag w -> Origin -> Destination -> Weight w -> Dag w
addEdge a b c d
    | (not.null) $ cycleDetect $ addEdge' a b c d = error $ "Cycle detected!"
    | otherwise = addEdge' a b c d  

-- |Helper function that that adds an edge to the given dag
addEdge' :: Dag w -> Origin -> Destination -> Weight w ->  Dag w
addEdge' (Dag a b) c d e = Dag a (edge:b)
    where edge = Edge {origin = c, destination = d, eWeight = e}

-- |cycleDetect checks if the procvided dag contains a cycle.
--In case of a cycle, the function returns a list of lists with
--the cyclic vertices.
cycleDetect :: Dag w -> [[Int]]
cycleDetect dag = filter ((>1).length)
                 $ map (\[(a,as), (b,bs)] -> (a `intersect` bs) ++ (b `intersect`as))
                 $ combs 2 $ map (getDestinations (edges dag)) (getOrigins (edges dag))

-- |Helper function for the cycle detection that produces a list
--of list for all possible pathes
combs 0 _ = [[]]
combs _ [] = []
combs k (x:xs) = map (x:) (combs (k-1) xs) ++ combs k xs

-- |topoSort creates a topological sorting of the provided dag. 
--The resulting list of Int is based on the Id's of the vertices.
topoSort :: Dag w -> [Int]
topoSort dag = reverse $ foldl topoSort' [] $ map (getDestinations (edges dag)) (getOrigins (edges dag))

-- |topoSort helper function 
topoSort' :: [Int] -> ([Int],[Int]) -> [Int]
topoSort' ts (x,xs)  = nub $ case elemIndex (head x) ts of
                                          Just i  -> uncurry(++) $ first(++xs) $ splitAt i ts
                                          _       -> ts ++ xs ++ x

-- |getDestinations is a helper function for topoSort
--it finds for a list of edges all possible direct destination vertices
getDestinations :: [Edge w] -> Int -> ([Int], [Int])
getDestinations edges chosenOrigin = ([chosenOrigin], map (destination) 
                                     $ filter (\edge -> origin edge == chosenOrigin) edges)

-- |getOrigins is a helper function for topoSort
--it returns for a list of edges a Int list which represent the
--id's of all edge origin vertices
getOrigins :: [Edge w] -> [Int]
getOrigins edges = foldl (\acc x -> (origin x):acc ) [] edges 

-- |weightLongestPath determines the weight of the longest path in a given dag
--the function requires the accessor functions WeightVertex and WeightEdge to
--be provided as arguments.
weightLongestPath :: (Plus w, Ord w, Comp w) => Dag w -> Int -> Int -> WeightVertex w -> WeightEdge w -> Weight w
weightLongestPath a b c wV wE = (Weight (pathCost a wV wE $ longestPath a b c wV wE))

-- |longestPath determines the longest/most expensive path from start to end
--The function taakes a dag, start and end id aswell as the weight extraction functions
--It calls pathList which gerates all possible pathes, using a toposorted, start/end chopped
--list as input. 
longestPath :: (Plus w, Ord w, Comp w) => Dag w -> Int -> Int -> WeightVertex w -> WeightEdge w -> [Int]
longestPath a b c wV wE = maximumBy (customComparing (pathCost a wV wE)) $ pathList a $ chopStartEnd b c $ topoSort a

-- |pathCost calculates the weight of a path
--The path is provided as a list of Int that represent the vertex
--id's of the path to calculate 
pathCost :: (Plus w) => Dag w -> WeightVertex w -> WeightEdge w -> [Int] -> w
pathCost a wV wE b
    |length b == 0 = error "empty path"
    |length b == 1 = extractor $ (Weight (wV a (b!!0)))
    |length b == 2 = extractor $ plus  (wE a (b!!0) (b!!1)) (extractor(plus ((wV a (b!!0))) ((wV a (b!!1))))) 
    |otherwise = pathCost' a (tail b) (plus (wE a (b!!0) (b!!1)) (wV a (b!!0))) wV wE

-- |helper function to calculate the weight of a given path. 
pathCost' :: (Plus w) => Dag w -> [Int] -> Weight w -> WeightVertex w -> WeightEdge w -> w
pathCost' a b c d e
    |length b == 0 = extractor c
    |length b == 1 = extractor $ plus (extractor c)  (d a (b!!0)) 
    |otherwise     = pathCost' a (tail b) (plus  (extractor(c)) (extractor(plus (d a (b!!0)) (e a (b!!0) (b!!1)))) )  d e

-- |pathList function is used to build assemble a list with vertex ids of potential
--pathes. Used to determine the longest path. 
pathList :: Dag w -> [Int] -> [[Int]]
pathList a b = pathList' a (reverse b) [[last b]] 

-- |helper function for pathList. Uses list comprehension to produce all possible
--combinations of pathes.
pathList' :: Dag w -> [Int] -> [[Int]] -> [[Int]]
pathList' a b c 
    | length b == 1 = filter (possible a) [b++x | x <- c]
    | length (incomingVertices a (head b)) == 0 = pathList' a (tail b) c 
    | otherwise = pathList' a (tail b) $ filter (possible a) $ nub $ concat $ [[x:y] ++ c | x <- (incomingVertices a (head b)), y <- c]

-- |chopStartEnd function discards all list entries before start and after end
--parameter. Uses removeUpTo function.
chopStartEnd :: Int -> Int -> [Int] -> [Int]
chopStartEnd a b c = reverse $ removeUpTo b $ reverse (removeUpTo a c)

-- |removeUpTo function removed in a list all elements before the provided value
removeUpTo :: Int -> [Int] -> [Int]
removeUpTo a (x:xs)
    | a == x = (x:xs)
    | otherwise   = removeUpTo a xs

-- |helper function that returns a list of all vertex id's from which
--the vertex with the provided id has incoming connections
incomingVertices :: Dag w -> Int -> [Int]
incomingVertices a b  = map origin (filter (\edge -> destination edge == b) (edges a))

-- |takes a dag and a number sequence and checks
--if it is valid edge sequence in the dag a
possible :: Dag w -> [Int] -> Bool
possible a b = possible' a b b

-- |helper function that recursively checks a 
--vertex id list whether the sequence would be
--possible given the available edges. 
possible' :: Dag w -> [Int] -> [Int] -> Bool
possible' a b c
    | length c == 1 = True
    | length (filter (\edge -> origin edge == head c && destination edge == ((tail c)!!0)) (edges a)) /= 0 = possible' a b (tail c)
    | otherwise = False

