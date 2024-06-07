structure Communication = struct
    fun send message sock = 
        let val serializedMessage = Word8VectorSlice.full (Byte.stringToBytes message)
        in print "Sending...\n"; Socket.sendVec (sock, serializedMessage)
        end

    fun receive amount sock = 
        let val message = Socket.recvVec (sock, amount)
        in print "Receiving...\n"; print (Byte.bytesToString message)
        end
	      
    fun receiveUntil (delimiter: Char.char) (sock: Socket.active INetSock.stream_sock): string =
	let val delimiter = str delimiter
	    fun loop acc = 
		let val message = (Byte.bytesToString (Socket.recvVec (sock, 1)))
		in if delimiter = message
		   then acc
		   else loop (acc ^ message)
		end
	in loop ""
	end
	    
    fun accept (handler: Socket.active INetSock.stream_sock -> unit) (serv: Socket.passive INetSock.stream_sock) = (
        print "Accepting";
        let val (s, _) = Socket.accept serv
            fun closure (): unit = (handler s; Socket.close s)
            val thread = Thread.Thread.fork (closure, [])	       
	in accept handler serv
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
