structure Payload = struct
    open Utilities
    
    type 'hash Descriptor =
        { hashes: 'hash list, fileName: string }
    fun 'hash create_descriptor hashList fileName : 'hash Descriptor =
        { hashes = hashList, fileName = fileName }
    datatype Action
            = OPEN of {filename: string}
		    | READ of {filename: string, quantity: int, offset: string option}
		    | WRITE of {filename: string, content: Word8Vector.vector}
    (* TODO: Upgrade to a decent parsing strategy: s-exprs, scheme *)

    structure WRITE = struct
        fun deserialize (content: string) =
            let val (filename, content) = String.splitAt (String.implode (List.tl (String.explode content))) (Char.chr 2)
            in WRITE {filename = filename, content = Byte.stringToBytes content}
            end
    end

    structure OPEN = struct
        fun deserialize (content: string) =
            OPEN {filename = (String.implode (List.tl (String.explode content)))}
    end
    
    fun deserialize (content: string): Action = (
        if content = "" then raise Fail "Nothing to be deserialized" else ();
	    let val tag = String.sub (content, 0)
	    in case tag
	        of #"O" => OPEN.deserialize content
	        |  #"W" => WRITE.deserialize content
	        | _ => raise Fail "Only OPEN is implemented xD"
	    end
    )
end


(* XFS *)
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
    
(* Glusterfsd *)
functor FileSystem (Hash: sig type hash end) = struct
    fun fopen (fileName: string) = 
        Payload.create_descriptor [] fileName
    fun fread (fileName: string) (quantity: int) (offset: Hash.hash option) = 
        raise Fail ":("
    fun fwrite (filename: string, content: Word8Vector.vector) =
        Data.set filename 0 content
end

structure Hashable = struct 
    type hash = Word.word
    type content = int
    val chunkSize = 10
    fun hashContent (x: content): hash = Word.fromInt x
    fun hashHash (left: hash, right: hash): hash = Word.+ (left, right)
end

(* Glusterd *)
functor BookKeeper (Hashable : sig type content
                                   type hash
                                   val chunkSize: int
                                   val hashContent : content -> hash
                                   val hashHash: hash * hash -> hash end) = struct
    type Location = {node: string, path: string, position: int, size: int}
    
    val files: Hashable.hash HashArray.hash =
        HashArray.hash 10 (* File -> Hash *)
    
    val locations: Location list HashArray.hash =
        HashArray.hash 10 (* Hash -> Locations *)
    
    structure Tree = MerkleTree (Hashable)

    structure FileSystem = FileSystem(Hashable)

    fun assureDirectory () = (
        if OS.FileSys.isDir "/tmp/canteen"
        then ()
        else OS.FileSys.mkDir "/tmp/canteen"
    )
    handle OS.SysErr _ => OS.FileSys.mkDir "/tmp/canteen"
        
    fun run () = (
        assureDirectory();
        let val buffer: string ref = ref ""
            val EOT = Char.chr 4
            (* handle Fail ex => (print (PolyML.makestring ex); loop ()) *)
            fun spawner (): unit = Communication.spawn (Communication.receiveUntil EOT buffer) 7778 5
            val thread = Thread.Thread.fork (spawner, [])
            fun loop () = ()
        in if Thread.Thread.isActive thread then loop () else ()
        end
    )
    
end
