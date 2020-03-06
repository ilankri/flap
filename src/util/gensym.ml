let make prefix =
  let r = ref 0L in
  fun () ->
    r := Int64.add !r 1L;
    Format.sprintf "%s%Ld" prefix !r
