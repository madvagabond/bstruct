exception Out_of_bounds



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


let set_writer_index t off =
  t.write <- off


let set_reader_index t off =
  t.read <- off


let skip t len =
  t.read <- t.read + len



(** buffer_capacity - writer_index *)
let writable t =
  capacity t - t.write







let create size =
  let buf = Cstruct.create size in
  let read = 0 in
  let write = 0 in
  {buf; read; write; initial_size = size}



let of_cstruct buf =
  let size = Cstruct.len buf in 
  let read = 0 in
  let write = size in
  {buf; read; write; initial_size = size}



let empty () =
  create 0




let clear t =
  t.write <- 0;
  t.read <- 0;
  t.buf <- Cstruct.create t.initial_size;;





let is_readable t len =
  let bytes = t.write - t.read - len in
  bytes >= 0



let read_bytes t len =
  let has_space = is_readable t len in

  if has_space then 
    Cstruct.sub t.buf t.read len
  else
    raise Out_of_bounds




  
  
let read_slice t len =
  read_bytes t len |> of_cstruct












let set t len fn c =
  let needed = t.write + len in
  let size = capacity t in 


  let _ =
    if size < needed then
      let diff = needed - size in
      grow t diff
    else
      ()
  in

  fn t.buf t.write c;
  t.write <- t.write + len








let get t n fn =

  if is_readable t n then 

    let i = fn t.buf t.read in
    let _ = t.read <- t.read + n in
    i

  else
    raise Out_of_bounds











module type BYTE_ORDER = sig
  open Cstruct 

  val set_uint32: Cstruct.t -> int -> uint32 -> unit
  val set_uint16: Cstruct.t ->  int -> uint16 -> unit
  val set_uint64: Cstruct.t -> int -> uint64 -> unit

  val get_uint16: Cstruct.t -> int -> uint16
  val get_uint32: Cstruct.t -> int -> uint32
  val get_uint64: Cstruct.t -> int -> uint64
    
end



module Make (O: BYTE_ORDER) = struct
  let set_uint32 t i =
    set t 4 (O.set_uint32) i

  let set_uint16 t i =
    set t 2 (O.set_uint16) i

  let set_uint64 t i =
    set t 8 (O.set_uint64) i


  let get_uint16 t =
    get t 2 O.get_uint16


  let get_uint32 t =
    get t 4 O.get_uint32
  
  let get_uint64 t =
    get t 8 O.get_uint64

end




module LE = Make(Cstruct.LE)
module BE = Make(Cstruct.BE)


