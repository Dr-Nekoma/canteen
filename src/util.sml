structure Utilities = struct
    infix 3 <|
    fun f <| x = f x
    infix 3 |>
    fun y |> f = f y
    infix 3 <||
    fun f <|| (x, y) = f x y (* Left section      *)
    infix 3 ||>
    fun (x, y) ||> f = f x y (* Left application  *)


    structure String = struct
        open String
    
        fun splitAt initialString splitByChar =
            let fun loop elem (acc: string) =
                let val tail = String.implode (List.tl (String.explode elem))
                    val head = String.sub(elem, 0)
                in
                    if splitByChar = head
                    then (acc, tail)
                    else loop tail (acc ^ Char.toString head)
                end
            in loop initialString "" end
    end

end
