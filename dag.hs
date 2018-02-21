import Data.List


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




