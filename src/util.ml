open Batteries
;;

let file_to_string f =
  BatIO.read_all (open_in f)
;;

(*
let dbg fmt4 =
  let print_and_flush s = begin
    output_string stderr s;
    flush stderr
  end
  in Printf.ksprintf2 print_and_flush fmt4
*)
let dbg fmt4 = Printf.ifprintf stderr fmt4
;;

let dbg2 fmt4 =
  let print_and_flush s = begin
    output_string stderr s;
    flush stderr
  end
  in Printf.ksprintf2 print_and_flush fmt4

let findi_all f xs =
  let rec do_ix i acc =
    if i >= 0 then
      if f (xs.(i)) then
        do_ix (i-1) (i::acc)
      else
        do_ix (i-1) acc
    else
      acc
  in do_ix (Array.length xs - 1) []
;;
