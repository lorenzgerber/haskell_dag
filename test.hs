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

p = topoSort o 
q = longestPath o 5 4 getWeightVertex getWeightEdge
r = weightLongestPath o 5 4 getWeightVertex getWeightEdge 

-- example data with cycle
u = addVertex (Dag [][]) (Weight 1)
v = addVertex u (Weight 2)
w = addEdge v 0 1 (Weight 3)
x = addEdge w 1 0 (Weight 4) 

-- example 1  from lab Assignment
s1 = addVertex (Dag [][]) (Weight "c") --a, 0
s2 = addVertex s1 (Weight "c")         --b, 1
s3 = addVertex s2 (Weight "c")         --c, 2
s4 = addVertex s3 (Weight "c")         --d, 3
s5 = addVertex s4 (Weight "c")         --e, 4
s6 = addVertex s5 (Weight "c")         --f, 5
s7 = addEdge s6 0 1 (Weight "c")
s8 = addEdge s7 0 2 (Weight "c")
s9 = addEdge s8 1 3 (Weight "a")
s10 = addEdge s9 2 3 (Weight "b")
s11 = addEdge s10 2 4 (Weight "a")
s12 = addEdge s11 3 5 (Weight "d")
s13 = addEdge s12 4 5 (Weight "a")

sSort = topoSort s13
sLongest = longestPath s13 0 5 getWeightVertex getWeightEdge
sWeight = weightLongestPath s13 0 5 getWeightVertex getWeightEdge


-- example 2 from lab assignment
n1 = addVertex (Dag [][]) (Weight (4::Int)) --a, 0
n2 = addVertex n1 (Weight 5)               --b. 1
n3 = addVertex n2 (Weight 3)               --c, 2
n4 = addVertex n3 (Weight 2)               --d, 3
n5 = addVertex n4 (Weight 7)               --e, 4
n6 = addEdge n5 0 1 (Weight 2)
n7 = addEdge n6 0 2 (Weight 2)
n8 = addEdge n7 1 3 (Weight 1)
n9 = addEdge n8 2 3 (Weight 1)
n10 = addEdge n9 2 4 (Weight 1)
n11 = addEdge n10 3 4 (Weight 3)

nSort = topoSort n11
nPath = longestPath n11 0 4 getWeightVertex getWeightEdge
nWeight = weightLongestPath n11 0 4 getWeightVertex getWeightEdge
