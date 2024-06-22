functor MerkleTree () =
struct
    fun pairwise [] = []
      | pairwise [x] = [(x, NONE)]
      | pairwise (x::y::xs) = (x, SOME y) :: pairwise xs

    fun generateHashedElements hash (leavesData: string list) =
      let val pairs = pairwise leavesData
      in List.rev(List.foldl (fn ((x, SOME y), acc) => 
                            (hash(x, y))::acc
                      | ((x, NONE), acc) => x::acc) [] pairs)
      end
    fun buildTree [elem] = elem
      | buildTree [] = raise Fail "Can't build tree with empty list"
      | buildTree leavesData = 
          let val current = generateHashedElements String.^ leavesData
          in buildTree current
          end
end
