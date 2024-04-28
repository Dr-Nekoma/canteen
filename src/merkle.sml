functor MerkleTree (Key : sig type t val equal : t * t -> bool end) =
struct
    datatype 'data t 
        = Node of { key: Key.t, children: 'data t array ref, data: 'data }
        | Nil
    fun search (keyToFind: Key.t) (tree: 'data t) =
        case tree of
          Nil => NONE
        | Node {key,children,...} =>
            if Key.equal (key, keyToFind)
            then SOME tree
            else Array.find(fn c => case search keyToFind c of SOME _ => true | NONE => false) (!children)
    fun insert (key: Key.t) (data: 'data) (tree: 'data t): 'data t = 
        case tree of
          Nil => Node {key = key, children = ref (Array.fromList []), data = data}
        | Node {key, children, data} => Node {key = key, children = ref (Array.fromList []), data = data}
end