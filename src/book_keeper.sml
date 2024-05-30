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
            let val value = BinIO.inputN (inStream, size)
	    in (BinIO.closeIn inStream; value)
	    end)
        end
    fun set entity position toWrite: unit =
        let val outStream = BinIO.openOut entity
        in (seek (Out outStream) position;
            BinIO.output (outStream, toWrite);
	    BinIO.flushOut outStream;
	    BinIO.closeOut outStream)
        end
end
    
(* Glusterfsd *)
functor FileSystem (Hash: sig type hash end) = struct
    fun fopen (fileName: string) = 
        Payload.create_descriptor [] fileName
    fun fread (fileName: string) (quantity: int) (offset: Hash.hash option) = 
        raise Fail ":("
    fun fwrite (filename: string, content: Word8Vector.vector) = (
	print filename;
	print (PolyML.makestring content);
        Data.set filename 0 content
    )
end

structure Hashable = struct 
    type hash = Word8.word
    type content = int
    val chunkSize = 10
    fun hashEncoder (a: hash) = (Word8Vector.fromList [a], 4) 
    fun hashContent (x: content): hash = Word8.fromInt x
    fun hashHash (left: hash, right: hash): hash = Word8.+ (left, right)
end

(* Glusterd *)
functor BookKeeper (Hashable : sig type content
                                   type hash
                                   val chunkSize: int
				   val hashEncoder : hash -> Word8Vector.vector * int
                                   val hashContent : content -> hash								    
                                   val hashHash: hash * hash -> hash end) = struct
    type Location = {node: string, path: string, position: int, size: int}
    
    val files: Hashable.hash HashArray.hash =
        HashArray.hash 10 (* File -> Hash *)
    
    val locations: Location list HashArray.hash =
        HashArray.hash 10 (* Hash -> Locations *)
    
    structure Tree = MerkleTree (Hashable)

    structure FileSystem = FileSystem(Hashable)

    fun filesTableEncoder (table: Hashable.hash HashArray.hash) : Word8Vector.vector =
	let fun foldAux (key, value, acc) =
		let val stringLength = String.size key
		    val encodedKey = [Word8Vector.fromList [Word8.fromInt stringLength]] @ [Byte.stringToBytes key]
		    val (encodedValue, encodedSize) = Hashable.hashEncoder value
                    val encodedFullValue = [Word8Vector.fromList [Word8.fromInt encodedSize]] @ [encodedValue]
		in encodedKey @ encodedFullValue @ acc
		end
	in Word8Vector.concat (HashArray.fold foldAux nil table)
	end

    fun locationsTableEncoder (table: Location list HashArray.hash) : Word8Vector.vector =
	let fun foldAux (key, value, acc) =
		let val stringLength = String.size key
		    val encodedKey = [Word8Vector.fromList [Word8.fromInt stringLength]] @ [Byte.stringToBytes key]
		    val (encodedValue, encodedSize) = Hashable.hashEncoder value
                    val encodedFullValue = [Word8Vector.fromList [Word8.fromInt encodedSize]] @ [encodedValue]
		in encodedKey @ encodedFullValue @ acc
		end
	in Word8Vector.concat (HashArray.fold foldAux nil table)
	end
	    
    fun serialize filepath data encoder : unit =
	let val serializedData = encoder data
        in Data.set filepath 0 serializedData				     
        end

    fun deserialize filepath decoder =
	let val data = Data.get filepath 0 (Position.toInt (OS.FileSys.fileSize filepath))
	in decoder data
	end

    fun assureDirectory () = (
        if OS.FileSys.isDir "/tmp/canteen"
        then ()
        else OS.FileSys.mkDir "/tmp/canteen"
    )
    handle OS.SysErr _ => OS.FileSys.mkDir "/tmp/canteen"
        
    fun run () = (
        assureDirectory();
        let val EOT = Char.chr 4
	    fun action (buffer: string): unit = 
		let val command = Payload.deserialize buffer
		in case command
		    of Payload.OPEN _ => raise Fail "Bomb"
		     | Payload.READ _ => raise Fail "Bomb"
		     | Payload.WRITE {filename = filename, content = content} =>
		       (* TODO: Add here a check for the chunkSize and the merkle tree structure *)
		       FileSystem.fwrite("/tmp/canteen/" ^ filename, content)
		end
            fun handler socket: unit = action(Communication.receiveUntil EOT socket)  			       
        in Communication.spawn handler 7778 5
        end
    )
    
end
