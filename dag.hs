import Control.Monad.State

data Vertex = Vertex Id Weight [ Edge ] deriving (Show)
data Edge = Edge Weight Vertex Vertex deriving (Show)
data Dag = Dag ([ Vertex ], [ Edge ])   

type Id = Int
type Weight = Int


data Person = Person {
  id   :: Int,
  name :: String
} deriving Show

startState = 0

type PersonManagement = State Int

generatePersonId :: PersonManagement Int
generatePersonId = do
    n <- get
    put (n+1)
    return n

createPerson :: String -> PersonManagement Person
createPerson name = do
    id <- generatePersonId
    return $ Person id name

runPersonManagement :: PersonManagement a -> a
runPersonManagement m = evalState m startState


work :: PersonManagement (Person, Person)
work = do
    john <- createPerson "John"
    steve <- createPerson "Steve"
    return (john, steve)

main = do
    let (john, steve) = runPersonManagement work
    putStrLn $ show john
    putStrLn $ show steve
 



--addVertex :: Dag -> Weight -> Dag

