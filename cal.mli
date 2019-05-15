type instruction =
  | Reset of int
  | Incr of int
  | Set of int * int
  | Jump of int * int * int

type expression =
  | Entier of int
  | Somme of expression * expression
  | If_then_else of expression * expression * expression

type programme = instruction array
exception Memory_Exhausted
exception Segmentation_Fault
exception Resources_Exhausted

val string_of_prog : programme -> unit
val print_registers : int array -> int -> unit
val run_instruction : int array -> instruction -> int -> int
val step_program : int array -> programme -> int -> int array
val debug_program : programme -> int list -> bool -> int
val rho : programme -> int
val normalize : programme -> programme
val clean_program : programme -> programme
val translate_jump :
  instruction array ->
  instruction array ->
  int ->
  int ->
  int ->
  instruction array
val compose : programme -> programme -> programme
val translate : programme -> int list -> int -> programme
val make_list : int -> int -> int list
val general_compose : programme -> programme list -> int -> programme
val if_then_else : programme -> programme -> programme -> programme
val prog_of_expr : expression -> programme

val prog_succ : programme
val prog_somme_2_args : programme
val prog_somme_3_args : programme
val prog_somme_modify : programme
val prog_constant : programme
val prog_constant_v2 : int -> programme
val prog_bigger : programme
val prog_moins_1 : programme
val prog_division_2 : programme
val prog_test_0 : programme
val prog_somme_dizaine_unite : programme