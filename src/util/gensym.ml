let make prefix =
  let r = ref 0 in
  fun () ->
    incr r;
    prefix ^ string_of_int !r
