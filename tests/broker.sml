(* main.sml *)

(* fun writeData () = *)
(*     Data.set "./data/Account.ndf" 0 (Word8Vector.tabulate (10, Word8.fromInt)) *)
(* handle Fail x => print x *)

infix 3 <|
  fun f <| x = f x
infix 3 |>
  fun y |> f = f y
infix 3 <||
  fun f <|| (x, y) = f x y (* Left section      *)
infix 3 ||>
  fun (x, y) ||> f = f x y (* Left application  *)

fun readData () =
    Data.get "./abc" 0 10
    |> Byte.bytesToString

fun writeData () =
    Data.set "./abc" 0 (Word8Vector.tabulate (10, Word8.fromInt))

fun main () = (
    writeData ();
    print (PolyML.makestring (readData ()) ^ "\n")
)
