fun main () =
    let val buffer = ref ""
        val _ = Communication.spawn (Communication.receiveUntil #"\r" buffer) 7778 5
    in print (PolyML.makestring (Payload.deserialize (!buffer)))
    end
