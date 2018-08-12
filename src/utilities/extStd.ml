(** This module extends some modules of the standard library. *)

module Ref = struct

  let as_functions default =
    let r = ref default in
    (fun x -> r := x), (fun () -> !r)

end

module List = struct

  include List

  (** [flat_map f l] is equivalent to [flatten (map f l)].  *)
  let flat_map f l = flatten (map f l)

  let rec range start stop =
    if stop < start then [] else start :: range (start + 1) stop

  let asymmetric_map2 f =
    let rec aux accu xs ys =
      match xs, ys with
      | xs, [] ->
        (List.rev accu, xs, [])
      | [], ys ->
        (List.rev accu, [], ys)
      | x :: xs, y :: ys ->
        aux (f x y :: accu) xs ys
    in
    aux []

  let rec uniq = function
    | [] -> []
    | [x] -> [x]
    | x :: ((y :: _) as xs) -> if x = y then uniq xs else x :: uniq xs

  (** [index_of p l] returns the index of the first element [x] of [l]
      such [p x = true]. Raise [Not_found] otherwise. *)
  let index_of : ('a -> bool) -> 'a list -> int =
    fun p l ->
      let rec aux i = function
        | [] -> raise Not_found
        | x :: xs -> if p x then i else aux (succ i) xs
      in
      aux 0 l

  (** [all_distinct ls] returns true if all the elements of [ls]
      are distinct. *)
  let all_distinct ls =
    let ls = List.sort compare ls in
    let rec aux = function
      | [] | [_] -> true
      | x :: y :: ys -> x <> y && aux (y :: ys)
    in
    aux ls

  (** [find_duplicate l] returns a duplicate element in [l]. Raise
      [Not_found] if there is no such element.  *)
  let find_duplicate l =
    let l = List.sort compare l in
    let rec aux = function
      | [] | [_] -> raise Not_found
      | x :: y :: ys ->
        if x = y then x else aux (y :: ys)
    in
    aux l

  let all_equal ls =
    let rec aux = function
      | [] | [_] -> true
      | x :: y :: ys -> x = y && aux (y :: ys)
    in
    aux ls

  let unique_value ls =
    match uniq ls with
    | [x] -> Some x
    | _ -> None

  let foldmap f init =
    let rec aux (accu, ys) = function
      | [] ->
        (accu, List.rev ys)
      | x :: xs ->
        let accu, y = f accu x in
        aux (accu, y :: ys) xs
    in
    aux (init, [])

  exception FoldMap2

  let foldmap2 f init l1 l2 =
    let rec aux (accu, ys) = function
      | [], [] ->
        (accu, List.rev ys)
      | x :: xs, z :: zs ->
        let accu, y = f accu x z in
        aux (accu, y :: ys) (xs, zs)
      | _, _ ->
        raise FoldMap2
    in
    aux (init, []) (l1, l2)

  let update_assoc k v l =
    let rec aux = function
      | [] -> [(k, v)]
      | ((k', v') as x) :: l -> if k = k' then (k, v) :: l else x :: aux l
    in
    aux l

  module Monad : sig
    type 'a t
    val return : 'a -> 'a t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val take_one : 'a list -> 'a t
    val fail : 'a t
    val and_try : 'a t -> 'a t -> 'a t
    val run : 'a t -> 'a list
  end = struct
    type 'a t = 'a list
    let return x = [x]
    let ( >>= ) x f = List.(flatten (map f x))
    let fail = []
    let and_try a b = a @ b
    let run x = x
    let take_one x = x
  end

end

let update
    (find : 'k -> 'c -> 'v)
    (add : 'k -> 'v -> 'c -> 'c)
    (k : 'k) (m : 'c)
    (default : 'v)
    (f : 'v -> 'v)
  : 'c =
  try
    let v = find k m in
    add k (f v) m
  with Not_found ->
    add k (f default) m

module Random = struct

  let int_in_range start stop =
    start + Random.int (stop - start + 1)

end

module Option = struct

  let map f = function
    | None -> None
    | Some x -> Some (f x)

end
