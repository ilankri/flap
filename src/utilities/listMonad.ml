type 'a t = 'a list

let pick cs = cs

let return a = [a]

let fail = []

let ( >>= ) m f =
  List.(flatten (map f m))

let run m = m
