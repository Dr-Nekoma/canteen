structure Node = BookKeeper(Hashable)

fun main () =
    print (PolyML.makestring (Node.run()))
handle ex => print (PolyML.makestring ex)

(*
datanode 1 file
0,7 garbage => 0 => filename1
7,5 trash => 0 => filename2
*)
