(* main.sml *)

fun writeData () =
    Data.set "/tmp/perplexdb/Account.ndf" 0 (Word8Vector.tabulate (10, Word8.fromInt))
handle Fail x => print x

fun readData () =
    Data.get "./abc" 0 1
handle IO.Io x => (
    print (PolyML.makestring x ^ "\n");
    NONE
)

fun main () = 
    print (PolyML.makestring (readData ()) ^ "\n")

