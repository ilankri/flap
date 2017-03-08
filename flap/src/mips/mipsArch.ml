(** Architectural information about MIPS *)

type register =
  | Ra
  | Sp
  | Fp
  | Gp
  | T of int
  | V of int
  | S of int
  | A of int

let ra = Ra

let sp = Sp

let fp = Fp

let gp = Gp

let t x =
  assert (x >= 0 && x <= 9);
  T x

let v x =
  assert (x >= 0 && x <= 1);
  V x

let s x =
  assert (x >= 0 && x <= 7);
  S x

let a x =
  assert (x >= 0 && x <= 3);
  A x

let all_registers = ExtStd.List.(List.(
  flatten [
    map t (range 0 9);
    map v [0];
    map a (range 0 3);
    map s (range 0 7)
  ]
))

let string_of_register = function
  | Sp -> "$sp"
  | Fp -> "$fp"
  | Gp -> "$gp"
  | Ra -> "$ra"
  | T i -> "$t" ^ string_of_int i
  | V i -> "$v" ^ string_of_int i
  | S i -> "$s" ^ string_of_int i
  | A i -> "$a" ^ string_of_int i

let string_of_register' = function
  | Sp -> "sp"
  | Fp -> "fp"
  | Gp -> "gp"
  | Ra -> "ra"
  | T i -> "t" ^ string_of_int i
  | V i -> "v" ^ string_of_int i
  | S i -> "s" ^ string_of_int i
  | A i -> "a" ^ string_of_int i

exception InvalidMIPSRegisterName of string

let register_of_string = function
  | "$fp" ->
    Fp
  | "$sp" ->
     Sp
  | "$gp" ->
     Gp
  | "$ra" ->
     Ra
  | s when Str.(string_match (regexp "\\$t\\([0-9]\\)") s 0) ->
     t (int_of_string (Str.matched_group 1 s))
  | s when Str.(string_match (regexp "\\$a\\([0-3]\\)") s 0) ->
     a (int_of_string (Str.matched_group 1 s))
  | s when Str.(string_match (regexp "\\$v\\([0-1]\\)") s 0) ->
     v (int_of_string (Str.matched_group 1 s))
  | s when Str.(string_match (regexp "\\$s\\([0-7]\\)") s 0) ->
     S (int_of_string (Str.matched_group 1 s))
  | s ->
     raise (InvalidMIPSRegisterName s)

let argument_passing_registers = ExtStd.List.(List.(
  map a (range 0 3)
))

let callee_saved_registers = ExtStd.List.(List.(
  Ra :: map s (range 0 7)
))

let caller_saved_registers = ExtStd.List.(List.(
  map t (range 0 9)
))

let return_register =
  v 0

let return_address_register =
  Ra

let tmp1 =
  v 1

let tmp2 =
  fp
