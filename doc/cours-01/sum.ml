let sum start stop body =
  let r = ref 0 in
  for i = start to stop do
    r := !r + body i
  done;
  !r

let main =
  let r = sum 1 10000 (fun x -> sum 1 10000 (fun y -> x + y)) in
  Printf.printf "%d\n%!" r

