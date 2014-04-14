open Batteries
;;
open Types
;;

(* Add the given entry to the map, unless that key is already present, in which
 * case indicate the conflict by storing None for that key.
 * ('a, 'b option) Map.t -> 'a -> 'b -> ('a, 'b option) Map.t *)
let map_add_conflict m x y =
  if Map.mem x m then
    Map.add x None m
  else
    Map.add x (Some y) m
;;

(* ast1 <Ast1_*leaf> -> string *)
let leaf_contents ast =
  match ast with
  | Ast1_parenthesized_leaf l -> l.contents
  | Ast1_backquoted_leaf l -> l.contents
  | Ast1_leaf l -> l.contents
  | _ -> assert false
;;

let build_globals ast =
  (* Look up name of module currently being compiled. *)
  let curmod =
    match ast.node with
    | Ast1_module (Some m, _, _) -> leaf_contents m
    | _ -> "Main" (* Default for unnamed modules *)
  in
  (* Return a list of all the top-level identifiers bound by the given
   * top-level declaration.
   * ast1 <node=Ast1_topdecl_*> -> string list *)
  let get_topdecl_names ast =
    match ast.node with
    | Ast1_topdecl_
  (* Given some info about a module's imported names, add those names to the
   * global environment. Add name conflicts where appropriate. Note that we
   * don't bother with helpful error messages for name conflicts.
   * The info is:
   * - prefix to add these names to globals under (the "as" module)
   * - bool for whether or not to add the corresp unqualified names
   * - triples (namespace, rawname, real_source_module) of things to add
   *   (note that real source module for a name might require a bunch of nested
   *   imports to dig up, but this is taken care of in get_import_names)
   * globals -> string * bool * (namespace * string * string) list -> globals *)
  and add_names glb (pfx, add_unq, names) =
    List.fold_left
      (fun g (pn,srcmod) ->
        if add_unq then
          { g with qualified = map_add_conflict g.qualified (pn,pfx)
                                 (Name_global (pn,srcmod));
                   unqualified = map_add_conflict g.unqualified pn
                                 (Name_global (pn,srcmod))}
        else
          { g with qualified = map_add_conflict g.qualified (pn,pfx)
                                 (Name_global (pn,srcmod))})
      glb
      names
  in
  match ast.node with
  | Ast1_module (_, _, { node = Ast1_body topdecls; _ }) ->
      let mynames = List.concat (List.map get_topdecl_names topdecls)
      and imported_names = Manager.get_import_names ast
      List.fold_left add_names
        { curmod = cm; unqualified = Map.empty; qualified = Map.empty }
        ((cm,true,mynames)::imported_names)
  | _ -> assert false
;;

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
