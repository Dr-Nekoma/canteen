structure Communication = struct
    fun send message sock = 
        let val serializedMessage = Word8VectorSlice.full (Byte.stringToBytes message)
        in print "Sending...\n"; Socket.sendVec (sock, serializedMessage)
        end

    fun receive amount sock = 
        let val message = Socket.recvVec (sock, amount)
        in print "Receiving...\n"; print (Byte.bytesToString message)
        end

    datatype HandleAction = Stop | Continue

    fun bleh () = (*
        let val command = Payload.deserialize (!buffer)
               in case command
                  of Payload.OPEN _ => raise Fail "Bomb"
                  | Payload.READ _ => raise Fail "Bomb"
                  | Payload.WRITE {filename = filename, content = content} =>
                      (* TODO: Add here a check for the chunkSize and the merkle tree structure *)
                      FileSystem.fwrite("/tmp/canteen/" ^ filename, content)
               end
    *)
        print "Hello!"

    fun receiveUntil (delimiter: Char.char) buffer (sock: Socket.active INetSock.stream_sock) =
	let val delimiter = str delimiter
	    fun loop acc = 
		let val message = (Byte.bytesToString (Socket.recvVec (sock, 1)))
		in if delimiter = message
		   then acc
		   else loop (acc ^ message)
		end
	in buffer := loop ""; Socket.close sock; Continue
	end
	    
    fun accept (handler: Socket.active INetSock.stream_sock -> HandleAction) (serv: Socket.passive INetSock.stream_sock) = (
        print "Accepting";
        let val (s, _) = Socket.accept serv
        in case handler s
	    of Stop => ()
	    | Continue => (bleh ();accept handler serv)
        end
    )
        
    fun spawn handler port queueSize = (
        print "spawn";
        let val s: Socket.passive INetSock.stream_sock = INetSock.TCP.socket ()
        in Socket.Ctl.setREUSEADDR (s, true);
           Socket.bind(s, INetSock.any port);
           Socket.listen(s, queueSize);
           print (Int.toString port ^ ": Listening...\n");
           accept handler s
        end
    )
end
