open Batteries
;;


(* woohoo made up numbers *)
let HASHTABLE_SIZE = 500
;;

(* To store the previous unique identifier for each string we've seen. *)
let (ht : (string, int) Hashtbl.t) =
  ref (Hashtbl.create HASHTABLE_SIZE)
;;

let get s =
  if Hashtbl.mem !ht s then begin
    Hashtbl.modify s (fun x -> x + 1) !ht;
    Hashtbl.find !ht s
  end
  else begin
    Hashtbl.add !ht s 1;
    1
  end
;;

let reset () =
  ht := Hashtbl.create HASHTABLE_SIZE
;;
