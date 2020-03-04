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

module Make (S : State) = struct
  type state = S.t

  include Monad.Make(struct
      type 'a t = S.t -> 'a * S.t

      let return a = fun st -> (a, st)

      let bind m f =
        fun st ->
        let a, st = m st in
        f a st
    end)

  let get = fun st -> (st, st)

  let modify f = fun st -> return () (f st)

  let put st = modify @@ Fun.const st

  let run ~init m = m init
end
