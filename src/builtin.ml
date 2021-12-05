module L = Llvm
open Ast

module StringMap = Map.Make(String)

let built_in_funcs = [
  ("create_edge", [
    (Node, "start");
    (Node, "end"); 
    (Int, "bold");
    (Int, "r");
    (Int, "g");
    (Int, "b")], Edge) ]

let semant_built_in_funcs = [
  ("printi", [(Int, "x")], Int);
  ("printb", [(Bool, "x")], Int);
  ("printf", [(Float, "x")], Int);
  ("printc", [(Char, "x")], Int);
  ("prints", [(String, "x")], Int);
  ("create_node", [
    (Float, "x");
    (Float, "y"); 
    (Float, "radius");
    (Int, "r");
    (Int, "g");
    (Int, "b");
    (VoidPtr, "data")], Node) ] @ built_in_funcs

let decl_built_in_functions ltype_of_typ the_module =
  (* return llvalue *)
  let decl_built_in_func (name, formal_list, return_typ) =
    let func_t : L.lltype = 
      L.function_type (ltype_of_typ return_typ) (Array.of_list (List.map (fun (ty, _) -> ltype_of_typ ty) formal_list))
    in L.declare_function name func_t the_module
  in
  List.map (fun ((name, formal_list, return_typ) as func_sig) -> (name, decl_built_in_func func_sig)) built_in_funcs
