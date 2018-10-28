
open Alcotest
open Bstruct
    
let string_ops () =
  let test () =

    let str = Gen.string 1024 () in 
    let buf = Bstruct.empty () in
    let len = String.length str in

    
    let _ = Bstruct.write_string buf str in
    check int "writer index is correct" buf.write len;
    
    let s = Bstruct.read_string buf len in
    check string "string is correct" str s;
   
  in

  Gen.repeat 250 test



let cstruct_t =
  let pp = Fmt.of_to_string Cstruct.to_string in
  let eq l r = (Cstruct.to_string l) = Cstruct.to_string r in
  Alcotest.testable pp eq


let cs_check name e g =

  let _ =
    let (l1, l2) = Cstruct.len e, Cstruct.len g in
    Alcotest.(check int) (Fmt.strf "%s returns a buffer with the correct length" name) l1 l2
  in

  Alcotest.check cstruct_t (Fmt.strf "%s returns a buffer with the correct content" name) e g


let conversions () =


  let cs = Gen.cstruct 4096 () in
  let buf = Bstruct.of_cstruct cs in

  let _ = Bstruct.grow buf 100 in
  let got = Bstruct.to_cstruct buf in

  let _ = cs_check "to_cstruct" cs got in

  let got_1 = Bstruct.copy_to_cstruct buf  in
  cs_check "copy_to_cstruct" cs got_1






let byte_ops () =

  
 
  let test () = 
    let buf = Bstruct.empty () in
    let bytes = Gen.compose Gen.cstruct (Gen.range 1024 4096) in
    let len = Cstruct.len bytes in
    
    let _ = Bstruct.write_bytes buf bytes in
    check int "writer index is correct" buf.write len; 
    
    let buf1 = Bstruct.read buf len in
    cs_check "read" bytes buf1.buf;

    Bstruct.reset buf;
    let buf2 = Bstruct.slice buf len in
    cs_check "slice" bytes buf2.buf;
  in


  Gen.repeat 250 test

  
    





module type ORDER = sig
    
  val set_uint16: t -> Cstruct.uint16 -> unit
  val set_uint32: t -> Cstruct.uint32 -> unit
  val set_uint64: t -> Cstruct.uint64 -> unit


  val get_uint16: t -> Cstruct.uint16
  val get_uint32: t -> Cstruct.uint32
  val get_uint64: t -> Cstruct.uint64
end 



let integer_ops (module O: ORDER) () =
  let buf = Bstruct.empty () in

  let test () =
    let i8 = Random.int 255 in 
    let i16 = Random.int 65535 in
    let i32 = Random.int32 Int32.max_int in
    let i64 = Random.int64 Int64.max_int in

    Bstruct.set_uint8 buf i8; 
    O.set_uint16 buf i16;
    O.set_uint32 buf i32;
    O.set_uint64 buf i64;


    
    check int "reads int8" i8 (Bstruct.get_uint8 buf);
    check int "reads int16" i16 (O.get_uint16 buf);
    check int32 "reads int32" i32 (O.get_uint32 buf);
    check int64 "reads int64" i64 (O.get_uint64 buf)
  in
  Gen.repeat 250 test

    

let be_test () = integer_ops (module Bstruct.BE) ()

let le_test () = integer_ops (module Bstruct.LE) ()



let suite = [
  "String Operations", `Quick, string_ops ;
  "Conversions", `Quick , conversions;
  "Byte Operations", `Quick, byte_ops;
  "LE Operations", `Quick, le_test;
  "BE Operations", `Quick, be_test; 
]

let () =
  Alcotest.run "Bstruct suite" [
    "suite", suite;
  ]
