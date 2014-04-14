open Types

(* For a given module AST, return a list of all prenames imported by any
 * import declarations. List contains one entry per import statement, of the
 * form:
   * [use prefix for this import (affected by as keyword)],
   * [whether to also add unqualified names (affected by qualified kwd)],
   * [list of pairs (real_source_module, prename)]
 * Note that the real source module might be different from the module directly
 * imported by an import statement (that module might have itself imported) *)
val get_import_names : ast1 -> (string * bool * (string * prename) list) list
