(* Handle the generic hash type later, no patience now *)

(* 
functor MerkleTree (Data : sig type 'hash t val hashContent : 'hash t -> 'hash val hashHash: 'hash * 'hash -> 'hash end) =
struct
    datatype 'hash t =
          Nil
        | Node of { hash: 'hash, left: 'hash t, right: 'hash t }
        | Leaf of { hash: 'hash, content: 'hash Data.t }
    fun 'hash buildRoot [] = Nil
      | buildRoot (x::[]) = Leaf { hash = Data.hashContent x, content = x }
      | buildRoot (leavesData: ('hash Data.t) list) =
        let val leavesData = if List.length leavesData mod 2 = 0 then leavesData else leavesData @ [List.last leavesData]
            val (left, right) = (buildRoot (List.take(leavesData, Int.div(List.length leavesData, 2))), buildRoot(List.take(List.rev leavesData, Int.div(List.length leavesData, 2))))
        in case (left, right) of
             (Node {hash = leftHash,...}, Node { hash = rightHash,...})
           | (Leaf {hash = leftHash,...}, Node { hash = rightHash,...})
           | (Leaf {hash = leftHash,...}, Leaf { hash = rightHash,...})
           | (Node {hash = leftHash,...}, Leaf { hash = rightHash,...}) => Node { hash = Data.hashHash(leftHash, rightHash), left = left, right = right }
           | (Leaf _, Nil) => left
           | (Nil, Leaf _) => right
           | (Node _, Nil)
           | (Nil, Node _)
           | (Nil, Nil) => Nil
        end
end *)

functor MerkleTree (Data : sig type t val hashContent : t -> string val hashHash: string * string -> string end) =
struct
    datatype 'hash t =
          Nil
        | Node of { hash: 'hash, left: 'hash t, right: 'hash t }
        | Leaf of { hash: 'hash, content: Data.t }
    fun 'hash buildTree [] = Nil
      | buildTree (x::[]) = Leaf { hash = Data.hashContent x, content = x }
      | buildTree (leavesData: Data.t list) =
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