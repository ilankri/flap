type ('k, 'v) dict

type ('k, 'v) t = ('k, 'v) dict

val empty : ('k, 'v) dict

val lookup : 'k -> ('k, 'v) dict -> 'v option

val insert : 'k -> 'v -> ('k, 'v) dict -> ('k, 'v) dict

val to_list : ('k, 'v) dict -> ('k * 'v) list

val of_list : ('k * 'v) list -> ('k, 'v) dict

val equal : ('k, 'v) dict -> ('k, 'v) dict -> bool
