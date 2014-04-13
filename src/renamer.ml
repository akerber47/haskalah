open Batteries
;;
open Types
;;

let build_globals ast =
  (* Return a list of all the top-level identifiers bound by the given
   * top-level declaration.
   * ast1 <node=Ast1_topdecl_*> -> string list *)
  let get_topdecl_names ast =
    (*TODO*)
  (* Given some info about a module's imported names, add those names to the
   * global environment. Add name conflicts where appropriate. Note that we
   * don't bother with helpful error messages for name conflicts.
   * The info is:
   * - prefix/qualifier to assign these names when adding to globals
   * - bool for whether or not to add the corresp unqualified names
   * - pairs (real_source_module, name) of things to add
   *   (note that real source module for a name might require a bunch of nested
   *   imports to find, but this is taken care of in get_import_names)
   * globals -> string * bool * (string * string) list -> globals *)
  and add_names glb (prefix, add_unq, names) =

  in
  (* Look up name of module currently being compiled. *)
  let cm =
    match ast.node with
    | Ast1_module (Some m, _, _) -> m
    | _ -> "Main" (* Default for unnamed modules *)
  in
  match ast.node with
  | Ast1_module (_, _, { node = Ast1_body topdecls; _ }) ->
      let mynames = List.concat (List.map get_topdecl_names topdecls)
      and imported_names = Manager.get_import_names ast
      foldl add_names
        { curmod = cm; unqualified = Map.empty; qualified = Map.empty }
        ((cm,true,mynames)::imported_names)


type environment = (string, string) Map.t

(* Look up unqualified identifier. Error if ambiguous. None if not found.
 * globals -> environment -> string -> string option *)
let lookup_unq glb env s =
  if Map.mem s env then
    Some (Map.find s env)
  else if Map.mem s glb then
    match Map.find s glb with
    | Some v -> Some v
    | None -> raise (Parse_error (Printf.sprintf2
        "Identifier '%s' is ambiguous\n" s))
  else
    None
;;

(* Similarly, qualified identifier. Note that we don't need to pass in a local
 * environment as these can only be declared at top-level. *)
let lookup_q glb m s =
  if Map.mem (m,s) glb then
    match Map.find (m,s) glb with
    | Some v -> Some v
    | None -> raise (Parse_error (Printf.sprintf2
        "Identifier '%s.%s' is ambiguous\n" m s))
  else
    None
;;
