structure Payload = struct
    type 'hash Descriptor =
        { hashes: 'hash list, fileName: string }
    fun 'hash create_descriptor hashList fileName : 'hash Descriptor =
        { hashes = hashList, fileName = fileName }
end 

functor FileSystem (Hash: sig type hash end ) = struct
    fun fopen (fileName: string) = 
        Payload.create_descriptor [] fileName
    fun fread (fileName: string) (quantity: int) (offset: Hash.hash option) = 
        raise Fail ":("
end

structure Hashable = struct 
    type hash = Word.word
    type content = int
    fun hashContent (x: content): hash = Word.fromInt x
    fun hashHash (left: hash, right: hash): hash = Word.+ (left, right)
end

functor BookKeeper (Hashable : sig type content
                                  type hash
                                  val hashContent : content -> hash
                                  val hashHash: hash * hash -> hash end) = struct
    val files: Hashable.hash HashArray.hash = HashArray.hash 10
    val hashes: string list HashArray.hash = HashArray.hash 10
    structure Tree = MerkleTree (Hashable)
end

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
