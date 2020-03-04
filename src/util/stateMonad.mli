module type State = sig
  type t
end

module type S = sig
  type state

  include Monad.S

  val get : state t

  val modify : (state -> state) -> unit t

  val put : state -> unit t

  val run : init:state -> 'a t -> 'a * state
end

module Make (S : State) : S with type state = S.t
