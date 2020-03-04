module type Minimal = sig
  type 'a t

  val return : 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type S = sig
  include Minimal

  val map : 'a t -> ('a -> 'b) -> 'b t

  val join : 'a t t -> 'a t

  module Infix : sig
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

    val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
  end
end

module Make (M : Minimal) = struct
  include M

  let map m f = bind m (fun v -> return (f v))

  let join m = bind m Fun.id

  module Infix = struct
    let ( >>= ) m f = bind m f

    let ( >|= ) m f = map m f
  end
end
