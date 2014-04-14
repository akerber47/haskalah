open Batteries
;;
open Types
;;

let get_import_names ast =
  (* Stub: TODO implement actual imports *)
  [(Prelude, true,
    List.map (fun s -> ("Prelude",s)) Fake_prelude.prelude_names)]
;;
