functor MerkleTree (Data : sig type content
                               type hash
                               val hashContent : content -> hash
                               val hashHash: hash * hash -> hash end) =
struct
    fun pairwise [] = []
      | pairwise [x] = [(x, NONE)]
      | pairwise (x::y::xs) = (x, SOME y) :: pairwise xs

    fun generateHashedElements (leavesData: Data.hash list) =
      let val pairs = pairwise leavesData
      in List.rev(List.foldl (fn ((x, SOME y), acc) => 
                            (Data.hashHash(x, y))::acc
                      | ((x, NONE), acc) => x::acc) [] pairs)
      end
    fun buildTree [elem] = elem
      | buildTree [] = raise Fail "Can't build tree with empty list"
      | buildTree leavesData = 
          let val current = generateHashedElements leavesData
          in buildTree current
          end
end
