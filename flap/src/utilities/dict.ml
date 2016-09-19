type ('k, 'v) dict =
    ('k * 'v) list

type ('k, 'v) t = ('k, 'v) dict

let empty = []

let lookup k d =
  try
    Some (List.assoc k d)
  with Not_found ->
    None

let insert k v d =
  (k, v) :: d

let to_list d = d

let of_list d = d

let equal d1 d2 =
  List.for_all (fun (k, v) ->
    lookup k d2 = Some v
  ) d1
  && List.(length d1 = length d2)
