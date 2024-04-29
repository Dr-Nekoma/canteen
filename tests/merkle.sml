structure Tree = MerkleTree (struct type t = int fun hashContent (x: t) = PolyML.makestring x fun hashHash (left: string, right: string) = left ^ right end)
fun main () =
    let val tree = Tree.buildTree [1, 2, 3, 4, 5]
    in (print ((PolyML.makestring tree) ^ "\n"))
    end
