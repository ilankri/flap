type location = int

type 'a block = {
  mutable tag : int;
  cells      : 'a array;
}

type 'a memory = {
  mutable bound : int;
  data          : 'a block option array;
}

type 'a t = 'a memory

let create size = {
  bound = 0;
  data  = Array.make size None
}

exception OutOfMemory

let allocate mem size init =
  if mem.bound >= Array.length mem.data then
    raise OutOfMemory
  else (
    let location = mem.bound in
    mem.data.(location) <- Some { tag = 0; cells = Array.make size init };
    mem.bound <- mem.bound + 1;
    location
  )

exception InvalidDereference of location

let dereference mem location =
  match mem.data.(location) with
    | None -> raise (InvalidDereference location)
    | Some b -> b

let read block i =
  block.cells.(i)

let write block i x =
  block.cells.(i) <- x

let get_tag block =
  block.tag

let set_tag block x =
  block.tag <- x

let print_location x = "#" ^ string_of_int x
