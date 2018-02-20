import Data.List


data Vertex = Vertex Id Weight deriving (Show)
data Edge = Edge Weight Vertex Vertex deriving (Show)
data Dag = Dag {vertices :: [ Vertex ], edges :: [ Edge ]} deriving (Show)   

type Id = Int
type Weight = Int

addVertex :: Dag -> Weight -> Dag
addVertex (Dag v w) weight = Dag (vertex:v) w
    where vertex = Vertex ((length v)+97) weight
