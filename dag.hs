import Control.Monad.State
import System.Random
import Data.UUID

data Vertex = Vertex Id Weight [ Edge ] deriving (Show)
data Edge = Edge Weight Vertex Vertex deriving (Show)
data Dag =  Dage ([ Vertex ], [ Edge ])   

type Id = Int
type Weight = Int

--addVertex :: Dag -> Weight -> Dag

