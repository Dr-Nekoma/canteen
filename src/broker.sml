structure Can :> sig
  val handleClient: Payload -> unit
  val handleServer: Payload -> unit
  val start: unit -> unit
  val close: unit -> unit
end = struct
end

(* 
structure Payload = struct
end 
*)

structure Data :> sig
  val get: string -> Position.int -> int -> Word8Vector.vector
  val set: string -> Position.int -> Word8Vector.vector -> unit
end = struct
    datatype Stream = In of BinIO.instream | Out of BinIO.outstream
    fun seek (stream: Stream ) position =
        case stream
        of In stream =>
            let val something = BinIO.StreamIO.getReader(BinIO.getInstream stream)
            in case something
            of reader as (BinPrimIO.RD{setPos = SOME setPos, ...}, _) => 
                (setPos position;
                BinIO.setInstream(stream, BinIO.StreamIO.mkInstream reader))
            | _ => raise Fail "Error in seek"
            end
        | Out stream => 
            let val something = BinIO.StreamIO.getWriter(BinIO.getOutstream stream)
            in case something
            of writer as (BinPrimIO.WR{setPos = SOME setPos, ...}, _) => 
                (setPos position;
                BinIO.setOutstream(stream, BinIO.StreamIO.mkOutstream writer))
            | _ => raise Fail "Error in seek"
            end
    fun get entity initialOffset size =
        let val inStream = BinIO.openIn entity
        in (seek (In inStream) initialOffset;
            BinIO.inputN (inStream, size))
        end
    fun set entity position toWrite =
        let val outStream = BinIO.openOut entity
        in (seek (Out outStream) position;
            BinIO.output (outStream, toWrite))
        end
end
