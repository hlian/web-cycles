## ~hello~

this takes the strongly connected components in the graph of requires and prints it out. each component gets printed out as a tree. whereas in a regular `printTree` the leaves simply end, here we annotate the leaves to show the edge from the leaf into the graph that creates the cycle that allowed the node to be admitted into the strongly connected component in the first place. each node is a file; each edge is a require from that file to another. the edges are directed. i think this is all the information you need for now?

## ~how to run~

* `cabal sandbox init`
* `cabal install --only-dep -j`
* `cabal run`

## ~some facts about strongly-connected components that i wrote down that helped me that might help you too~

* they're a property of directed graphs (in an undirected graph there's one strongly-connected component and it contains all the vertices)
* every cycle is part of a strongly connected component (proof: a, b are in a cycle; b is connected to a; a is connected to b)
* SCC forms an equivalence relation (proof: a SCC a; a SCC b CC c implies a SCC c; a SCC b implies b SCC a), which means it can partition a graph
* an SCC contains at least one cycle, but usually more (example: a perfectly connected graph with n vertices is one SCC with O(exp(2, n)) cycles)
* it's NP-complete to find the longest cycle
* therefore it's NP-complete to enumerate all the cycles in a graph (as longest cycle reduces to enumerating and then finding the max)
* but we can do pretty well just by drawing out the SCC
* Haskell's Data.Graph lets you visualizes an SCC by taking giving you arbitrary minimum spanning tree and letting you print it out
* each leaf L in this tree must have an edge to some other node in the tree M corresponding to an edge (L, M) in the original graph
* thus each leaf represents the node "right before" a set of cycles
* which is why i decided to print out the neighbors of each leaf that intersect with the nodes in the SCC tree
* graph theory!

-hao
