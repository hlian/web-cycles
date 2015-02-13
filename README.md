## ~hello~

this takes the strongly connected components in the graph of requires and prints it out. each component gets printed out as a tree. whereas in a regular `printTree` the leaves simply end, here we annotate the leaves to show the edge from the leaf into the graph that creates the cycle that allowed the node to be admitted into the strongly connected component in the first place. each node is a file; each edge is a require from that file to another. the edges are directed. i think this is all the information you need for now?

## ~how to run~

* `cabal sandbox init`
* `cabal install --only-dep -j`
* `cabal run`

-hao
