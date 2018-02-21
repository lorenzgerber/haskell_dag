import Data.List


data Vertex w = Vertex {vId :: Int
                       , vWeight :: Weight w
                       } deriving (Show)

data Edge w = Edge { eWeight :: Weight w
                   , origin :: Int
                   , destination :: Int
                   } deriving (Show)

data Dag v e = Dag { vertices :: [ Vertex v ]
               , edges :: [ Edge e ]
               } deriving (Show)   

data Weight a = Weight a deriving (Show, Eq)

weightInt :: Int -> Weight Int
weightInt a = Weight a


addVertex :: Dag v e -> Weight v -> Dag v e 
addVertex (Dag a b) c = Dag (vertex:a) b
   where vertex = Vertex {vId = (length a), vWeight = c}  
 
