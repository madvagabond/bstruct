
type t = {
  mutable buf: Cstruct.t;
  mutable read: int;
  mutable write: int;
  initial_size: int; 
}



let grow t i =
  t.buf <- Cstruct.append t.buf (Cstruct.create i) 

let capacity t =
  Cstruct.len t.buf

(**  writer_index - reader_index*)
let readable t =
  t.write - t.read

let reader_index t = t.read
let writer_index t = t.write

(** buffer_capacity - writer_index *)
let writable t =
  capacity t - t.write


let get_slice t off len =
  Cstruct.sub t.buf off len 

let create size =
  let buf = Cstruct.create size in
  let read = 0 in
  let write = 0 in
  {buf; read; write; initial_size = size}


let of_cstruct buf =
  let size = Cstruct.len buf in 
  let read = 0 in
  let write = 0 in
  {buf; read; write; initial_size = size}

let empty () =
  create 0


let clear t =
  t.buf <- Cstruct.create t.initial_size;;


let is_readable t len =
  t.read > abs (t.write - len)


