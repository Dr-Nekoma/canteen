structure Tree = MerkleTree (struct type t = int fun equal (a: int, b: int) = a = b end)
fun main () =
    let 
        val tree = Tree.Node {key = 1, children = ref (Array.fromList [Tree.Node {key = 2, children = ref (Array.fromList []), data = 2}, 
                                                                                    Tree.Node {key = 3, children = ref (Array.fromList []), data = 3}]), data = 1}
    in (print ((PolyML.makestring (Tree.search 3 tree)) ^ "\n"))
    end
