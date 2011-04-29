open Circuit
open Solver
open Gsl_helpers
open Printf

module Input = struct
  type input_src_spec = {
    id       : int;
    pos      : circuit_node;
    neg      : circuit_node;
    src      : Source.AC.source;
  }
end

module Conductance = struct
  type nonlinear_conductance = {
    g1_fn    : float -> float;
    g2_fn    : float -> float;
    g3_fn    : float -> float;
    pos      : circuit_node;
    neg      : circuit_node;
  }
end

module Volterra = functor (Solver:SOLVER) -> struct
  (*
    Implements the method of non-linear currents
  *)
  module I = Input
  module C = Conductance
  open Gsl_helpers.ComplexHelpers

  (* This solves the system at a particular frequency w *)
  let solve_at w g_matrix c_matrix b_vec =
    let size = Gsl_vector_complex.length b_vec in
    let x_vec = Gsl_vector_complex.create ~init:zero size in
    let mna_matrix = Gsl_matrix_complex.copy c_matrix in
    Gsl_matrix_complex.scale mna_matrix (complex 0. w);
    Gsl_matrix_complex.add mna_matrix g_matrix;
    
    let permut = Gsl_permut.create size in
    ignore (Gsl_linalg.complex_LU_decomp (`CM mna_matrix) permut);
    Gsl_linalg.complex_LU_solve (`CM mna_matrix) permut (`CV b_vec)
      (`CV x_vec);
    x_vec
      
  let solve name s ~xinit ~(src1:I.input_src_spec)
      ~(src2:I.input_src_spec) ~conductance =
    let c = conductance in
    let size = Solver.size s in
    let voltage vec node = match node with
	`Ground -> 0.
      | `Node loc -> vec.(loc)
    in
    let vdc = voltage xinit c.C.pos in
    let g1 = conductance.C.g1_fn vdc
    and g2 = conductance.C.g2_fn vdc
    and g3 = conductance.C.g3_fn vdc
    in
    printf "vdc=%g g1=%g g2=%g g3=%g\n%!" vdc g1 g2 g3;
    (match c.C.neg with
	 `Ground -> ()
       | `Node _ -> failwith "non ground node not supported"
    );
    let pos_id = match c.C.pos with
	`Ground -> failwith "positive node cannot be grounded"
      | `Node loc -> loc
    in
    (* retrieve the exiting c_matrix and g_matrix in complex form *)
    let g_linear = Solver.get_ac_matrix s in
    let c_matrix = Solver.get_cc_matrix s in
    (* Add the g1 admittance to the g_matrix *)
    g_linear.{pos_id, pos_id} <-
      Gsl_complex.add g_linear.{pos_id, pos_id} (complex g1 0.);
    
    (* Step 1... solve DC... skip this for now *)

    (* Step 2: solver using b1 ... 1st order mixing products *)
    let w1, mag1 = src1.I.src.Source.AC.freq, src1.I.src.Source.AC.mag
    and w2, mag2 = src2.I.src.Source.AC.freq, src2.I.src.Source.AC.mag in
    let b_vec = Gsl_vector_complex.create ~init:zero size in
    b_vec.{pos_id} <- complex mag1 0.;
    let x1 = solve_at w1 g_linear c_matrix b_vec in
    b_vec.{pos_id} <- complex mag2 0.;
    let x2 = solve_at w2 g_linear c_matrix b_vec in
    print_complex_vector "x1" x1;
    print_complex_vector "x2" x2;

    (* Step 3: solve using b2 = g2 * (u1 ^ 2) .. 2nd order mixing products*)

    (* Step 4: solve using b3 = g3 * (u1 ^ 3) + g2 * u1 * u2 ... 3rd order *)
    (*Circuit.add_admittance *)

end;;
