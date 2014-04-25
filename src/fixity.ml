open Batteries
;;
open Types
;;

type fixity =
  | Left
  | Right
  | Non
;;

(* To avoid having to do a second (!) descent just to handle fixities, we store
 * all operator fixities (by name) in a global lookup table. Yeah, this is
 * really hacky / not functionally pure, but whatever. *)
let HASHTABLE_SIZE = 500
;;
(* (name, fixity * int) Hashtbl.t *)
let fixities = Hashtbl.create HASHTABLE_SIZE
;;

let declare ast =
  let (fx, prec, ops) =
    match ast.node with
    (* Pull out the precedence integer and parse it. Note that this is the ONLY
     * time we actually parse and convert (not just validate) literals in the
     * entire compiler, as we need an integer value at compile time. *)
    | Ast1_decl_fixity (fx, Some { node1 = Ast1_rleaf (
        Rleaf_literal (Lit_int, i)); _}, ops) ->
          let p =
            try int_of_string i with
            | _ ->
                raise (Fixity_error (Printf.sprintf2
                  "Invalid operator precedence '%s' at (%d,%d)"
                  i ast.blockstart1 ast.blockend1))
          in begin
            if p < 0 || p > 9 then
              raise (Fixity_error (Printf.sprintf2
                "Invalid operator precedence '%s' at (%d,%d)"
                i ast.blockstart1 ast.blockend1));
            (fx, p, ops)
          end
    (* Otherwise, assume default precedence of 9 *)
    | Ast1_decl_fixity (fx, None, ops) ->
        (fx, 9, ops)
    | _ -> assert false
  in
  let fixity =
    match fx.node with
    | Ast1_leaf { token = RInfixl; _} -> Left
    | Ast1_leaf { token = RInfixr; _} -> Right
    | Ast1_leaf { token = RInfix; _ } -> Non
    | _ -> assert false
  in
  (* Add all the given operators with the given fixity. If any operators
   * already have an assigned fixity, error. *)
  List.iter
    (fun (Ast1_rleaf (Rleaf_name opname)) -> begin
      if Hashtbl.mem fixities opname then
        raise (Fixity_error (Printf.sprintf2
          "Cannot redefine fixity of operator: at (%d,%d)"
          ast.blockstart1 ast.blockend1));
      Hashtbl.add fixities opname (fixity, prec)
    end)
    ops
;;

let resolve ast =
