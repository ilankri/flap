type menu =
  | Meal of string
  | And of menu * menu
  | Or of menu * menu

let happy_meal =
  And (And (
    Or (Meal "Sucre", Meal "Sel"),
    Or (Meal "Sucre salé", Meal "Sel sucré")),
       Meal "Glace")

type command =
  | RAnd  of command * command
  | RMeal of string

module NonDeterministMonad : sig
  type 'a t
  val return  : 'a -> 'a t
  val fail : 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val choice  : 'a list -> 'a t
  val run : 'a t -> 'a list
end = struct
  type 'a t = 'a list
  let fail = []
  let return x = [x]
  let ( >>= ) c f = List.flatten (List.map f c)
  let choice l = l
  let run x = x
end

open NonDeterministMonad

let rec make_command
: menu -> command NonDeterministMonad.t = function
  | Meal s ->
    return (RMeal s)
  | And (m1, m2) ->
    make_command m1 >>= fun c1 ->
    make_command m2 >>= fun c2 ->
    return (RAnd (c1, c2))
  | Or (m1, m2) ->
    make_command m1 >>= fun c1 ->
    make_command m2 >>= fun c2 ->
    choice [c1; c2] >>= fun c ->
    return c

let rec range start stop =
  if start > stop then [] else start :: range (start + 1) stop

 (* [ (a, b)
      for a in range (0, 10) and b in range (0, 10)
      if a + b = 10
    ] *)
let pairs =
  choice (range 0 10) >>= fun a ->
  choice (range 0 10) >>= fun b ->
  if a + b = 10 then return (a, b) else fail
