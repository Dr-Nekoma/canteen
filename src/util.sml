
structure Utilities = struct
  infix 3 <|
  fun f <| x = f x
  infix 3 |>
  fun y |> f = f y
  infix 3 <||
  fun f <|| (x, y) = f x y (* Left section      *)
  infix 3 ||>
  fun (x, y) ||> f = f x y (* Left application  *)
end		     
