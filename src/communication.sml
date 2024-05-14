structure Communication = struct
    fun send message sock = 
        let val serializedMessage = Word8VectorSlice.full (Byte.stringToBytes message)
        in print "Sending..."; Socket.sendVec (sock, serializedMessage); Socket.close sock
        end

    fun receive sock = 
        let val message = Socket.recvVec (sock, 6)
        in print "Receiving..."; print (Byte.bytesToString message)
        end

    fun accept (handler: Socket.active INetSock.stream_sock -> unit) (serv: Socket.passive INetSock.stream_sock) =
        let val (s, _) = Socket.accept serv
        in handler s; accept handler serv
        end

    fun spawn handler port queueSize =
        let val s: Socket.passive INetSock.stream_sock = INetSock.TCP.socket ()
        in Socket.Ctl.setREUSEADDR (s, true);
           Socket.bind(s, INetSock.any port);
           Socket.listen(s, queueSize);
           print (Int.toString port ^ ": Listening...\n");
           accept handler s
        end
end