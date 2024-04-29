structure Tree = MerkleTree (struct type hash = string type content = int fun hashContent (x: content) = PolyML.makestring x fun hashHash (left: hash, right: hash) = left ^ right end)
fun main () =
    let val tree = Tree.buildTree [1, 2, 3, 4, 5]
    in (print ((PolyML.makestring tree) ^ "\n"))
    end
