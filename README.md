## Bstruct, a resizable, mutable buffer built on cstruct. 

## How does it differ from mstruct?

It's more similar to Netty's bytebuf in the sense that it is resizeable, and has separate indexes for reads and writes.

So with Mstruct you can't write past it's inital allocated size, and you can't read bytes you've written without converting it to a cstruct, or shifting its offset manually.

It also has a number of zero copy read operations, read the docs or the mli file if you want to know more about them.




## Usage.
```ocaml

  (** allocating a buffer of size 0 *)
  let buf = Bstruct.create 0 in

  let _ = Printf.printf "%d\n" (Bstruct.capacity buf) in

  let _ = Bstruct.write_string buf "hello world" in
  let _ = Bstruct.read_string buf 11 |> print_endline in


  let _ = Bstruct.BE.set_uint64 buf 900L in
  let i = Bstruct.BE.get_uint64 buf in
  let _ = Printf.printf "%Ld\n" i in


  (** Copies to bstruct *)
  Bstruct.to_cstruct buf |> Cstruct.to_string |> print_endline


```
