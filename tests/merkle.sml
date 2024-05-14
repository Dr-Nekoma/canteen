structure Hashable = struct 
    type hash = string
    type content = int
    fun hashContent (x: content) = PolyML.makestring x
    fun hashHash (left: hash, right: hash) = left ^ right
end

structure Tree = MerkleTree (Hashable)

fun main () =
    let val tree = Tree.buildTree (List.map Hashable.hashContent [1, 2, 3, 3, 4, 5, 5, 5])
    in (print ((PolyML.makestring tree) ^ "\n"))
    end
