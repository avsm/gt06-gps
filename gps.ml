(* *)

module P = Proto
open Lwt.Infix
open Printf

let port = 8000

let newconn _sa fd =
  prerr_endline "new conn";
  let buf = Cstruct.create 512 in
  Lwt_cstruct.read fd buf >>= fun len ->
  prerr_endline (sprintf "read %d bytes" len);
  let resp = Cstruct.create 512 in
  prerr_endline (Cstruct_sexp.sexp_of_t buf |> Sexplib.Sexp.to_string_hum);
  let olen = 18 in
  Cstruct.set_uint8 resp 0 0x78;
  Cstruct.set_uint8 resp 1 0x78;

  Cstruct.set_uint8 resp 2 0x0D;

  Cstruct.set_uint8 resp 3 0x01;

  Cstruct.set_uint8 resp 4 0x01;
  Cstruct.set_uint8 resp 5 0x23;
  Cstruct.set_uint8 resp 6 0x45;
  Cstruct.set_uint8 resp 7 0x67;     
  Cstruct.set_uint8 resp 8 0x89;
  Cstruct.set_uint8 resp 9 0x01;
  Cstruct.set_uint8 resp 10 0x23;
  Cstruct.set_uint8 resp 11 0x45; 

  Cstruct.set_uint8 resp 12 0x00;
  Cstruct.set_uint8 resp 13 0x01;

  Cstruct.set_uint8 resp 14 0x8c;
  Cstruct.set_uint8 resp 15 0xdd;

  Cstruct.set_uint8 resp 16 0x0d;
  Cstruct.set_uint8 resp 17 0x0;

  let obuf,_ = Cstruct.split resp olen in
  Lwt_cstruct.write fd obuf >>= fun len ->
  prerr_endline (sprintf "wrote %d bytes" len);
  let rec read () =
    let buf = Cstruct.create 512 in
    Lwt_cstruct.read fd buf >>= fun len ->
    prerr_endline (sprintf "read %d bytes" len);
    prerr_endline (Cstruct_sexp.sexp_of_t buf |> Sexplib.Sexp.to_string_hum);
    read () in
  read ()

let t =
  let sa = Lwt_unix.(ADDR_INET (Unix.inet_addr_any, port)) in 
  Lwt_io.establish_server_with_client_socket sa newconn >>= fun _server ->
  Lwt.return_unit

let () = Lwt_main.run t
