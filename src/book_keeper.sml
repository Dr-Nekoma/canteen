type 'hash Descriptor =
        { hashes: 'hash list, fileName: string }
structure Payload = struct
    open Utilities
    
    
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



structure Hashable = struct 
    type hash = Word8Vector.vector
    type content = string
    val toContent = Byte.bytesToString
    val chunkSize = 10
    (* fun hashEncoder (a: hash) = (Word8Vector.fromList [a], 4)  *)
    val hashContent = Byte.stringToBytes
    val hashHash = Word8Vector.concat
    val serialize = Byte.stringToBytes
    fun equal (x: Word8Vector.vector, y: Word8Vector.vector) =
        Byte.bytesToString x = Byte.bytesToString y
end

(* XFS *)
structure Data :> sig 
  val get: string -> Position.int -> int -> Word8Vector.vector
  val set: string -> Position.int option -> Word8Vector.vector -> Position.int
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
    fun set entity (position: Position.int option) toWrite: Position.int =
        let val offset =
           case position 
           of NONE => OS.FileSys.fileSize entity
           | SOME v => v
        val outStream = BinIO.openOut entity
        in (seek (Out outStream) offset;
            BinIO.output (outStream, toWrite);
	    BinIO.flushOut outStream;
	    BinIO.closeOut outStream;
        offset)
        end
end
    
(* Glusterfsd *)
functor FileSystem (Hash : sig type content
                                type hash
                                val chunkSize: int
                                val toContent: Word8Vector.vector -> content
                                val hashContent : content -> hash
                                val hashHash: hash * hash -> hash
                                val serialize: content -> Word8Vector.vector
                                val equal: hash*hash -> bool end) = struct
    (*

    https://git.sr.ht/~mmagueta/relational-engine/tree/durability/item/src/main.cpp
        compute hash out of the content
        check if hashToReplace is NONE
        if empty then append to a disk file (/tmp/canteen/storage will do) getting the offset and size written
           and=> append to the end of the Hash.hash list of the filesTable in the entry of the filename
        else (hashToReplace is SOME) then copy 0..i where i = index of the hashToReplace, change the i position to the new container in the COPY and copy i+1..n
        create new location entry with the size written and offset
        add it linked to the new computed hash to the locations table
        return the location added
        call it a day xD
    *)
    type FilesTable = Hash.hash list HashArray.hash
    type Location = {position: Position.int, size: int}
    type LocationsTable = Location HashArray.hash
    fun assureFileExists (filename: string) = (
        
    )
    fun fopen (fileName: string, files: FilesTable): FilesTable = 
        let val realStoragePath = "/tmp/canteen/storage"
            val hashValue: Hash.hash list option = HashArray.sub (files, fileName)
            val emptyHash: Word8Vector.vector = Byte.stringToBytes "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
        in case hashValue
            of NONE => (HashArray.update (files, fileName, [Hash.hashContent (Hash.toContent emptyHash)]); files)
            | SOME _ => files
        end
    fun fread (fileName: string) (quantity: int) (offset: Hash.hash option) = 
        raise Fail ":("
    fun fwrite (files: FilesTable, locations: LocationsTable, fileName: string, content: Hash.content, hashToReplace: Hash.hash option): FilesTable * LocationsTable = 
        let val currentHashOfFile = HashArray.sub (files, fileName)
            val serializedContent = Hash.serialize content
            val hashedContent = Hash.hashContent content
            fun appendOnly (): unit =
                case currentHashOfFile 
                of SOME currentHashOfFile => HashArray.update (files, fileName, hashedContent::currentHashOfFile)
                |  NONE => raise Fail "Unreachable"
            fun appendInPlace (hashToReplace: Hash.hash) =
                case currentHashOfFile
                of SOME currentHashOfFile => 
                    HashArray.update (files, fileName, List.map (fn hash => if Hash.equal (hash, hashToReplace) then hashToReplace else hash) currentHashOfFile)
                |  NONE => raise Fail "Unreachable"
        in case hashToReplace
        of NONE => (
            appendOnly ();
            HashArray.update (locations, hashedContent, {position = Data.set fileName NONE serializedContent, size = Word8Vector.length serializedContent});
            (files, locations)
        )
        | SOME hashToReplace => (
            appendInPlace hashToReplace;
            HashArray.update (locations, hashedContent, {position = Data.set fileName NONE serializedContent, size = Word8Vector.length serializedContent});
            (files, locations)
        )
        end
end

(* Glusterd *)
functor BookKeeper (Hash : sig type content
                                   type hash
                                   val chunkSize: int
                                   val toContent: Word8Vector.vector -> content
                                   val hashContent : content -> hash
                                   val serialize: content -> Word8Vector.vector
                                   val equal: hash * hash -> bool
                                   val hashHash: hash * hash -> hash end) = struct
    type Location = {position: int, size: int}
    
    val files: Hashable.hash HashArray.hash =
        HashArray.hash 10 (* File -> Hash *)
    
    val locations: Location list HashArray.hash =
        HashArray.hash 10 (* Hash -> Locations *)
    
    structure Tree = MerkleTree (Hash)

    structure FileSystem = FileSystem(Hash)

    (* fun filesTableEncoder (table: Hashable.hash HashArray.hash) : Word8Vector.vector =
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
	end *)
	    
    fun serialize filepath data encoder : unit =
        let val serializedData = encoder data
        in Data.set filepath NONE serializedData				     
        end

    fun deserialize filepath decoder =
	let val data = Data.get filepath 0 (Position.toInt (OS.FileSys.fileSize filepath))
	in decoder data
	end
        
    fun run () = (
        Utilities.assureDirectory();
        let val EOT = Char.chr 4
	        fun action (buffer: string): unit =
		        let val command = Payload.deserialize buffer
                in case command
                    of Payload.OPEN _ => raise Fail "Bomb"
                    | Payload.READ _ => raise Fail "Bomb"
                    | Payload.WRITE {filename = filename, content = content} =>
                    (* TODO: Add here a check for the chunkSize and the merkle tree structure *)
                    (* (files: FilesTable, locations: LocationsTable, fileName: string, content: Hash.content, hashToReplace: Hash.hash option): FilesTable * LocationsTable *)
                        FileSystem.fwrite(files, locations, "/tmp/canteen/" ^ filename, Hash.toContent content, NONE)
                end
            fun handler socket: unit = action(Communication.receiveUntil EOT socket)
        in Communication.spawn handler 7778 5
        end
    )
    
end
