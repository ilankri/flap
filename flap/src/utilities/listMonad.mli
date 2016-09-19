(**

    The list monad
    or "non deterministic computations in OCaml"
    or "list comprehension in OCaml"

    As any monad, the purpose of the list monad is to represent in
    OCaml computations that are not directly expressible in OCaml.

    OCaml is a deterministic language: there is at most one value for
    each expression, i.e. at most one result for each computation.

    What if we want to represent computations that have zero, one
    or many results? For instance, imagine the following algorithm:

    pick x in {1..10}
    pick y in {1..10}
    return (x + y)

    This algorithm is non deterministic because "pick" takes one of
    the integers in the set {1..10}. Imagine that we want to know
    all possible executions of this program. How to do that?

    Before answering that question, you may wonder why it would be
    useful to write such a program and then ask for all its execution.
    The answer is: because that is exactly the syntax of sequences
    defined by comprehension! So, it is a concise and declarative way
    to represent a set of values defined by means of generators,
    combinaisons and filters. In other words, the previous program
    represents what a mathematician would write:

    { x + y | x ∈ [1..10], y ∈ [1.10] }

    Nice, isn't it?

    Now, let us come back to monads. In OCaml, there is no "pick"
    but we can program it. More generally, we can *represent*
    computations that are non deterministic as terms of type ['a t].

*)
type 'a t
(**

   A value of type ['a t] is a computation that may produce nothing or
   a value of type 'a or many values of type 'a.

*)

(** [pick s] is a non deterministic operation that takes one of the
    element of [s]. You do not know which one. *)
val pick : 'a list -> 'a t

(** [return x] is a non deterministic computation that evaluates
    into [x]. *)
val return : 'a -> 'a t

(** [fail] is a non deterministic computation with no result. *)
val fail : 'a t

(** [m >>= (fun x -> e)] is a computation that first executes
    [m], name its result [x] and then executes [e]. [x] may
    correspond to zero, one or many values of type ['a] but
    you consider it as a single potential value.
*)
val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

(** Now, here is how to write the previous program using these
    functions:

    let allsums =
      pick (range 0 10) >>= (fun x ->
      pick (range 0 10) >>= (fun y ->
        return (x + y)
      )

    (assuming [range start stop] is the list of integers between
    [start] and [stop]).

    Finally, how to get all these integers? Just use [run]:
*)
val run : 'a t -> 'a list

(**

    For instance, [run allsums] evaluates into:

   [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 2; 3; 4; 5;
    6; 7; 8; 9; 10; 11; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 4; 5; 6; 7; 8; 9; 10;
    11; 12; 13; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 6; 7; 8; 9; 10; 11; 12; 13;
    14; 15; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 8; 9; 10; 11; 12; 13; 14; 15;
    16; 17; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18]

*)
