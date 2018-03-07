module Test where
import Dag 

 
-- example data
a = addVertex (Dag [][]) (Weight (1::Int)) -- 0
b = addVertex a (Weight 2)          -- 1
c = addVertex b (Weight 3)          -- 2
d = addVertex c (Weight 4)          -- 3
e = addVertex d (Weight 5)          -- 4
f = addVertex e (Weight 6)          -- 5
g = addVertex f (Weight 7)          -- 6
h = addEdge g 5 6 (Weight 8)
i = addEdge h 6 1 (Weight 9)
j = addEdge i 0 1 (Weight 10)
k = addEdge j 0 2 (Weight 11)
l = addEdge k 1 3 (Weight 12)
m = addEdge l 2 4 (Weight 13)
n = addEdge m 2 3 (Weight 14)
o = addEdge n 3 4 (Weight 15)

p = weightLongestPath o 5 4 getWeightVertex getWeightEdge 

-- example data with cycle
u = addVertex (Dag [][]) (Weight 1)
v = addVertex u (Weight 2)
w = addEdge v 0 1 (Weight 3)
x = addEdge w 1 0 (Weight 4) 





