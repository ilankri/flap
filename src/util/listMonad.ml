include Monad.Make (struct
    type 'a t = 'a list

    let return a = [a]

    let bind m f = List.(flatten (map f m))
  end)

let pick cs = cs

let fail = []

let ( >>= ) m f = bind m f

let run m = m
