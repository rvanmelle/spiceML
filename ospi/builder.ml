open Printf
open Solver
open Circuit

module Builder = functor (Solver: SOLVER) -> struct
    (* This module is responsible for building a solver matrix in a
       manner independent of the solver matrix format or
       representation.  It assumes a class MNA matrix stamp for
       elements and makes calls on this basis *)

  let create c =
    let size = Hashtbl.length c.node_map in
    let solver = Solver.create size in
      Hashtbl.iter (fun (i,j) value ->
        (*let i, j = (map_node c i), (map_node c j) in*)
        debug "Add g val: %d,%d %g\n" i j value;
        Solver.set_a_matrix solver i j value) c.g_map;
    
    Hashtbl.iter (fun (i,j) value ->
      (*let i, j = (map_node c i), (map_node c j) in*)
      debug "Add c val: %d,%d %g\n" i j value;
      Solver.set_c_matrix solver i j value) c.c_map;
    
    Hashtbl.iter (fun i fn_list ->
      (*let i = map_node c i in*)
      debug "Add b val: %d\n" i;
      Solver.set_source_vec solver i fn_list) c.b_map;
    
    Hashtbl.iter (fun i fn_pairs ->
      debug "Add f vals: %d (%d)\n" i (List.length fn_pairs);
      List.iter (fun (fx, dfx) ->
	match dfx with
	  | Some dfx ->
	      Solver.add_nonlinear_eq solver i ~f:fx ~df:dfx ()
	  | None ->
	      Solver.add_nonlinear_eq solver i ~f:fx ()) fn_pairs) c.f_map;
    
    solver
      
end;;
