module Test where
import Dag 

 
-- example data
a = addVertex (Dag [][]) (Weight 'a') -- 0
b = addVertex a (Weight 'b')          -- 1
c = addVertex b (Weight 'c')          -- 2
d = addVertex c (Weight 'd')          -- 3
e = addVertex d (Weight 'e')          -- 4
f = addVertex e (Weight 'f')          -- 5
g = addVertex f (Weight 'g')          -- 6
h = addEdge g 5 6 (Weight 'h')
i = addEdge h 6 1 (Weight 'i')
j = addEdge i 0 1 (Weight 'j')
k = addEdge j 0 2 (Weight 'k')
l = addEdge k 1 3 (Weight 'l')
m = addEdge l 2 4 (Weight 'm')
n = addEdge m 2 3 (Weight 'n')
o = addEdge n 3 4 (Weight 'o')

p = weightLongestPath o 5 4 getWeightVertex getWeightEdge 

-- example data with cycle
u = addVertex (Dag [][]) (Weight 1)
v = addVertex u (Weight 2)
w = addEdge v 0 1 (Weight 3)
x = addEdge w 1 0 (Weight 4) 





