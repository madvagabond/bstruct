exception Out_of_bounds

type t = {
  mutable buf: Cstruct.t;
  mutable read: int;
  mutable write: int;
  initial_size: int; 
}


val empty: unit -> t
val create: int -> t 


val set_writer_index: t -> int -> unit
val set_reader_index: t -> int -> unit
  
val grow: t -> int -> unit


(** Number of readable bytes left which is writer_index - reader_index *)
val readable: t -> int


(** returns entire underlying cstruct*)
val underlying: t -> Cstruct.t 

(** Returns the actual size of the underlying buffer *)
val capacity: t -> int

(** Resets both reader index to 0 *)
val reset: t -> unit

(** Discard underlying cstruct and replaces it with another buffer of it's initial size*)
val clear: t -> unit

val writer_index: t -> int
val reader_index: t -> int
  
(** Reads len bytes of buffer without copying, and returns a new cstruct use with care.*)
val slice_bytes: t -> int -> Cstruct.t

(** Reads len bytes without copying and returns a new buffer, use with care.*)
val slice: t -> int -> t

(** Copies len bytes and  returns a new buffer*)
val read: t -> int -> t

(** Copies len bytes and returns a cstruct*)
val read_bytes: t -> int -> Cstruct.t

val read_string: t -> int -> string
  
(** append dst src, copies one buffers written bytes to another *)
val append: t -> t -> unit

val write_bytes: t -> Cstruct.t -> unit
val write_string: t -> string -> unit


val of_cstruct: Cstruct.t -> t

(** Returns a cstruct containing written bytes without copying*)
val to_cstruct: t -> Cstruct.t 

(** Copies the written bytes of the underlying buffer to a fresh cstruct *)
val copy_to_cstruct: t -> Cstruct.t 

val get_uint8: t -> int
val set_uint8: t -> int -> unit



module BE: sig
  
  
  val set_uint16: t -> int -> unit
  val set_uint32: t -> int32 -> unit
  val set_uint64: t -> int64 -> unit


  val get_uint16: t -> int
  val get_uint32: t -> int32
  val get_uint64: t -> int64
    
end


module LE: sig

  val set_uint16: t -> int -> unit
  val set_uint32: t -> int32 -> unit
  val set_uint64: t -> int64 -> unit


  val get_uint16: t -> int
  val get_uint32: t -> int32
  val get_uint64: t -> int64
    
    
end 
 
