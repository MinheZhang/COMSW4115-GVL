module L = Llvm
open Ast

module StringMap = Map.Make(String)

let built_in_funcs = [
  (* Edge Function *)
  ("create_edge", [
    (Node, "start");
    (Node, "end"); 
    (Int, "bold");
    (Int, "r");
    (Int, "g");
    (Int, "b")], Edge);
  ("edge_change_color", [
    (Edge, "e");
    (Int, "r");
    (Int, "g");
    (Int, "b")], Int);
  (* Get Edge Attributes *)
  ("get_edge_start", [(Edge, "e")], Node);
  ("get_edge_end", [(Edge, "e")], Node);
  ("get_edge_bold", [(Edge, "e")], Int);
  ("get_edge_r", [(Edge, "e")], Int);
  ("get_edge_g", [(Edge, "e")], Int);
  ("get_edge_b", [(Edge, "e")], Int);
  (* Set Edge Attributes *)
  ("set_edge_start", [(Edge, "e"); (Node, "start")], Int);
  ("set_edge_end", [(Edge, "e"); (Node, "end")], Int);
  ("set_edge_bold", [(Edge, "e"); (Int, "bold")], Int);
  ("set_edge_r", [(Edge, "e"); (Int, "r")], Int);
  ("set_edge_g", [(Edge, "e"); (Int, "g")], Int);
  ("set_edge_b", [(Edge, "e"); (Int, "b")], Int);
  (* Node Functions *)
  (* Get Node Attributes *)
  ("get_node_x", [(Node, "n")], Float);
  ("get_node_y", [(Node, "n")], Float);
  ("get_node_radius", [(Node, "n")], Float);
  ("get_node_r", [(Node, "n")], Int);
  ("get_node_g", [(Node, "n")], Int);
  ("get_node_b", [(Node, "n")], Int);
  (* TODO get_node_extra *)
  (* Set Node Attributes *)
  ("set_node_x", [(Node, "n"); (Float, "x")], Int);
  ("set_node_y", [(Node, "n"); (Float, "y")], Int);
  ("set_node_radius", [(Node, "n"); (Float, "radius")], Int);
  ("set_node_r", [(Node, "n"); (Int, "r")], Int);
  ("set_node_g", [(Node, "n"); (Int, "g")], Int);
  ("set_node_b", [(Node, "n"); (Int, "b")], Int);
  (* TODO set_node_extra *)
  (* ("set_node_extra", [(Node, "n"); (VoidPtr, "extra")], Int); *)
  ("set_node_color", [(Node, "n"); (Int, "r"); (Int, "g"); (Int, "b")], Int);
  (* Graph Functions*)
  ("create_graph", [], Graph);
  ("add_node", [(Graph, "g"); (Node, "n")], Int);
  ("remove_node", [(Graph, "g"); (Node, "n")], Int);
  ("add_edge", [(Graph, "g"); (Edge, "e")], Int);
  ("remove_edge", [(Graph, "g"); (Node, "n")], Int);
  ("destroy_graph", [(Graph, "g")], Int);
  ("get_edges", [(Graph, "g"); (Node, "n")], GvlList);
  (* TODO print_graph *)
  (* List Functions *)
  ("list_begin", [(GvlList, "l")], GvlListIterator);
  ("list_end", [], GvlListIterator);
  ("list_iter_next", [(GvlListIterator, "iter")], GvlListIterator);
  ("list_iter_data", [(GvlListIterator, "iter")], VoidPtr)
  ]

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
