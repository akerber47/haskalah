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

(* Look up the given prename in the given environment. Error if ambiguous.
 * None if not present.
 * environment -> prename -> name option *)
let lookup env pn =
  if Map.mem pn env then
    match Map.find pn env with
    | Some n -> Some n
    | None -> raise (Name_error begin
      match pn with
      | (_,Some pfx,s) ->
        Printf.sprintf2
          "Identifier '%s.%s' is ambiguous\n" pfx s
      | (_,_,s) ->
        Printf.sprintf2
          "Identifier '%s' is ambiguous\n" s
    end)
  else
    None
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
   * globals -> string * bool * (namespace * string * string) list -> globals
   * *)
  and add_names glbenv (pfx, add_unq, names) =
    List.fold_left
      (fun g (ns, rn, srcmod) ->
        if add_unq then
          map_add_conflict
            (map_add_conflict g (ns, Some pfx, rn)
                                (Name_global (pn,srcmod)))
            (ns, None, rn)
            (Name_global (pn,srcmod))
        else
          map_add_conflict g (ns, Some pfx, rn)
                             (Name_global (pn,srcmod))
      glbenv
      names
  in
  match ast.node with
  | Ast1_module (_, _, { node = Ast1_body topdecls; _ }) ->
      let mynames = List.concat (List.map get_topdecl_names topdecls)
      and imported_names = Manager.get_import_names ast
      List.fold_left add_names
        Map.empty
        ((cm,true,mynames)::imported_names)
  | _ -> assert false
;;

