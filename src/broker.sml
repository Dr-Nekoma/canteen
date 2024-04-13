structure Broker = struct
  fun entryPoint () = (
    print "Test\n";
    OS.Process.sleep (Time.fromSeconds 1);
    entryPoint ()
  )
end

(*
BEGIN Person Account
    PROJECT AccountNumber FROM Person SELECT Id = 1
    INTO UPDATE ...
END
*)

structure Data = struct
    fun get entity initialOffset size =
        let val inStream = BinIO.openIn entity
            val (BinPrimIO.RD{setPos = SOME setPos, ...}, _) = 
                 BinIO.StreamIO.getReader(BinIO.getInstream inStream)
        in (setPos (Position.fromInt initialOffset);
            BinIO.input1 (inStream))
        end
    fun set entity position payload =
        let val outStream = BinIO.openOut entity
            val (writer, _) = BinIO.StreamIO.getWriter(BinIO.getOutstream outStream)
        in case writer of
            BinPrimIO.WR{setPos = SOME setPos, block = _, ...} => 
                (setPos (Position.fromInt position);
                 BinIO.output(outStream, payload))
            | _ => raise Fail "Could not open the writer stream position setter."
        end
end