open Circuit

type jacobian = Gsl_matrix.matrix
type vec_type = Gsl_vector.vector
type matrix_type = Gsl_matrix.matrix

type newton_result_status = Converged | IterationLimitExceeded

module type SOLVER =
sig
  type t

  val create : int -> t
  val solve : t -> frequency -> unit
  val size : t -> int
  val solve_nonlinear : ?init_x:float array -> t -> newton_result_status
  val solve_nonlinear_noderiv : ?init_x:float array -> t -> newton_result_status
  val set_a_matrix : t -> int -> int -> float -> unit
  val set_c_matrix : t -> int -> int -> float -> unit
  val get_y_bar_matrix : t -> harmonics:int -> w:float -> Gsl_matrix.matrix
  val get_y_bar_matrix2 : t -> harmonics:int -> ws:(float list) -> Gsl_matrix.matrix
  val get_a_matrix : t -> Gsl_matrix.matrix
  val get_c_matrix : t -> Gsl_matrix.matrix
  val get_ac_matrix : t -> Gsl_matrix_complex.matrix
  val get_cc_matrix : t -> Gsl_matrix_complex.matrix
  val set_b_vec : ?alpha:float -> t -> source_func_input -> unit
  val get_b_vec : ?alpha:float -> t -> source_func_input -> vec_type
  val set_source_vec : t -> int -> source_func list -> unit
  val apply_nonlinear_functions : t -> x:vec_type -> y:vec_type -> unit
  val apply_nonlinear_derivatives : t -> x:vec_type -> j:jacobian -> unit
  val add_nonlinear_eq : t -> int -> f:f_type -> ?df:df_type -> unit -> unit
  val get_complex : t -> node_id -> Complex.t
  val get_real : t -> node_id -> float
  val get_solution : t -> float array
  val print : ?file:string -> t -> unit

end;;
