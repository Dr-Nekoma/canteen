(* TODO: Make tree non binary *)
(* TODO: Use drop instead of take and reverse *)
(* TODO: Handle hashes without duplications to avoid [1,2,3,4,5] == [1,2,3,3,4,5,5,5] *)
functor MerkleTree (Data : sig type content
                               type hash
                               val hashContent : content -> hash
                               val hashHash: hash * hash -> hash end) =
struct
    datatype t =
          Nil
        | Node of { hash: Data.hash, left: t, right: t }
        | Leaf of { hash: Data.hash, content: Data.content }
    fun buildTree [] = Nil
      | buildTree (x::[]) = Leaf { hash = Data.hashContent x, content = x }
      | buildTree (leavesData: Data.content list) =
        let val (left, right) = 
                if List.length leavesData mod 2 = 0
                then (buildTree(List.take(leavesData, Int.div(List.length leavesData, 2))), 
                      buildTree(List.take(List.rev leavesData, Int.div(List.length leavesData, 2))))
                else (buildTree(List.take(leavesData, Int.div(List.length leavesData, 2))), 
                      buildTree(List.take(List.rev leavesData, Int.div(List.length leavesData, 2)+1)))
        in case (left, right) of
             (Node {hash = leftHash,...}, Node { hash = rightHash,...}) => Node { hash = Data.hashHash(leftHash, rightHash), left = left, right = right }
           | (Leaf {hash = leftHash,...}, Node { hash = rightHash,...}) => Node { hash = Data.hashHash(leftHash, rightHash), left = left, right = right }
           | (Leaf {hash = leftHash,...}, Leaf { hash = rightHash,...}) => Node { hash = Data.hashHash(leftHash, rightHash), left = left, right = right }
           | (Node {hash = leftHash,...}, Leaf { hash = rightHash,...}) => Node { hash = Data.hashHash(leftHash, rightHash), left = left, right = right }
           | (Leaf _, Nil) => left
           | (Nil, Leaf _) => right
           | (Node _, Nil) => Nil
           | (Nil, Node _) => Nil
           | (Nil, Nil) => Nil
        end
end