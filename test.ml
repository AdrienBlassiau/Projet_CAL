open Urm
open Printf

exception Error of string * exn

let max_registers = 10
let r_tab = Array.make max_registers 0
let _ = Random.self_init ()

(***************************************************************************)
(***************************************************************************)
(***************************************************************************)
(************************** QUELQUES FONCTIONS *****************************)
(***************************************************************************)
(***************************************************************************)
(***************************************************************************)


(* [do_test (test_name, test_function, wanted_anwser, print_anwser)] fonction
   qui réalise le test test_name de la fonction test_function

   @requires  test_name le nom du test, test_function le nom de la fonction à
              tester, wanted_anwser la solution attendue et print_anwser la
              fonction d'affichage
   @ensures   le résultat est affiché sur la sortie standard
*)
let do_test (test_name, test_function, wanted_anwser, print_anwser) =
  try
    let res = test_function () in
    if res = wanted_anwser
    then
      Format.printf "%s : \027[32mOk\027[0m@." test_name
    else
      Format.printf
        "%s : Error : wanted:=%a obtained:=%a@."
        test_name print_anwser wanted_anwser print_anwser res
  with e ->
    Format.printf "%s : Uncaught exception %s@." test_name (Printexc.to_string e)

(* [fprintf_bool fmt b] fonction d'affichage des booléens

   @requires  fmt le format à afficher, b le booléen à afficher
   @ensures   le résultat est affiché sur la sortie standard
*)
let fprintf_bool fmt b = Format.fprintf fmt "%b" b

(* [fprintf_exn fmt e] fonction qui réalise le test test_name de la fonction
   test_function

   @requires  fmt le format à afficher, e l'exceptions à afficher
   @ensures   le résultat est affiché sur la sortie standard
*)
let fprintf_exn fmt e = Format.fprintf fmt "%s" (Printexc.to_string e)

(***************************************************************************)
(***************************************************************************)
(***************************************************************************)
(*********************** QUELQUES MATRICES DE TESTS ************************)
(***************************************************************************)
(***************************************************************************)
(***************************************************************************)


let tests_run_instruction_bool =
  [ "run_instruction_reset_i_test",
    (fun () ->
       run_instruction r_tab (Reset 0) 1 = 2),
    true, fprintf_bool;
    "run_instruction_reset_content_test",
    (fun () ->
       let _ = run_instruction r_tab (Reset 0) 1 in
       r_tab.(0) = 0),
    true, fprintf_bool;
    "run_instruction_incr_i_test",
    (fun () ->
       run_instruction r_tab (Incr 1) 6 = 7),
    true, fprintf_bool;
    "run_instruction_incr_content_test",
    (fun () ->
       let _ = run_instruction r_tab (Incr 1) 6 in
       r_tab.(1) = 2),
    true, fprintf_bool;
    "run_instruction_set_i_test",
    (fun () ->
       run_instruction r_tab (Set (1,0)) 9 = 10),
    true, fprintf_bool;
    "run_instruction_set_content_test",
    (fun () ->
       let _ = run_instruction r_tab (Set (1,0)) 9 in
       r_tab.(0) = 2),
    true, fprintf_bool;
    "run_instruction_jump_i_test",
    (fun () ->
       run_instruction r_tab (Jump (0,1,4)) 1 = 4),
    true, fprintf_bool;
  ]

let tests_run_instruction_exn  =
  [ "Memory_Exhausted",
    (fun () ->
       try
         ignore @@ run_instruction r_tab (Reset 11) 2;
         raise @@ Error ("Should raise",Memory_Exhausted)
       with e -> e),
    Memory_Exhausted, fprintf_exn;
    "Segmentation_Fault",
    (fun () ->
       try
         ignore @@ run_instruction r_tab (Jump (21,1,4)) 2;
         raise @@ Error ("Should raise",Segmentation_Fault)
       with e -> e),
    Segmentation_Fault, fprintf_exn;
  ]

let tests_step_program_bool =
  [ "step_program_succs",
    (fun () ->
       let r_tab = Array.make max_registers 0 in
       let max_steps = 1000 in
       let new_r_tab = step_program r_tab prog_succ max_steps in
       new_r_tab.(0) = 1),
    true, fprintf_bool;
    "step_program_prog_somme_dizaine_unite",
    (fun () ->
       let r_tab = Array.make max_registers 0 in
       let max_steps = 1000 in
       r_tab.(0) <- 91;
       let _ = step_program r_tab prog_somme_dizaine_unite max_steps in
       r_tab.(0) = 10),
    true, fprintf_bool;
  ]

let tests_run_instruction_exn =
  [ "Resources_Exhausted",
    (fun () ->
       try
         ignore @@
         step_program r_tab prog_somme_dizaine_unite 10;
         raise @@ Error ("Should raise",Resources_Exhausted)
       with e -> e),
    Resources_Exhausted, fprintf_exn;
  ]

let tests_debug_program_bool =
  [ "debug_program_succs",
    (fun () ->
       debug_program prog_succ [10] false = 11),
    true, fprintf_bool;
    "debug_program_prog_somme_2_args",
    (fun () ->
       debug_program prog_somme_2_args [97;3] false = 100),
    true, fprintf_bool;
    "prog_somme_3_args",
    (fun () ->
       debug_program prog_somme_3_args [1;2;3] true = 6),
    true, fprintf_bool;
    "debug_program_prog_constant",
    (fun () ->
       debug_program prog_constant [97] false = 97),
    true, fprintf_bool;
    "debug_program_prog_constant_v2",
    (fun () ->
       debug_program (prog_constant_v2 97) [0] false = 97),
    true, fprintf_bool;
    "debug_program_prog_bigger_1_0",
    (fun () ->
       debug_program prog_bigger [0;0] false = 1),
    true, fprintf_bool;
    "debug_program_prog_bigger_1_1",
    (fun () ->
       debug_program prog_bigger [1;0] false = 1),
    true, fprintf_bool;
    "debug_program_prog_bigger_0",
    (fun () ->
       debug_program prog_bigger [0;1] false = 0),
    true, fprintf_bool;
    "debug_program_prog_moins_1",
    (fun () ->
       debug_program prog_moins_1 [97] false = 96),
    true, fprintf_bool;
    "prog_division_2",
    (fun () ->
       debug_program prog_division_2 [98] false = 49),
    true, fprintf_bool;
    "prog_test_0_0",
    (fun () ->
       debug_program prog_test_0 [0] false = 0),
    true, fprintf_bool;
    "prog_test_0_1",
    (fun () ->
       debug_program prog_test_0 [1] false = 1),
    true, fprintf_bool;
    "debug_program_prog_somme_dizaine_unite",
    (fun () ->
       debug_program prog_somme_dizaine_unite [97] true = 16),
    true, fprintf_bool;
  ]

let tests_rho =
  [ "rho_prog_succ",
    (fun () ->
       rho prog_succ = 0),
    true, fprintf_bool;
    "rho_prog_somme_dizaine_unite",
    (fun () ->
       rho prog_somme_dizaine_unite = 4),
    true, fprintf_bool;
    "rho_prog_moins_1",
    (fun () ->
       rho prog_moins_1 = 2),
    true, fprintf_bool;
  ]

let tests_normalize =
  [ "normalize_prog_succ",
    (fun () ->
       let prog = normalize prog_somme_modify in
       prog.(3) = Jump (1, 2, 5)
    ),
    true, fprintf_bool;
  ]

let tests_clean_program =
  [ "clean_program_prog_somme_2_args",
    (fun () ->
       let prog = clean_program prog_somme_2_args in
       prog.(5) = Reset 1 && prog.(6) = Reset 2
    ),
    true, fprintf_bool;
  ]

let tests_translate_jump =
  [ "translate_jump_prog_somme_2_args",
    (fun () ->
       let prog = translate_jump prog_somme_2_args (Array.copy prog_somme_2_args) 0 5 42 in
       prog.(0) = Jump (1, 2, 47) &&
       prog.(3) = Jump (1, 2, 47) &&
       prog.(4) = Jump (1, 1, 43)
    ),
    true, fprintf_bool;
  ]

let tests_compose =
  [ "compose_prog_succ",
    (fun () ->
       let prog = compose prog_succ prog_succ in
       debug_program prog [4] false = 6
    ),
    true, fprintf_bool;
    "compose_compose_prog_succ",
    (fun () ->
       let prog = compose prog_succ (compose prog_succ prog_succ) in
       debug_program prog [4] false = 7
    ),
    true, fprintf_bool;
  ]

let tests_translate =
  [ "compose_prog_succ",
    (fun () ->
       let prog = translate prog_somme_dizaine_unite [7;8;9] 2 in
       string_of_prog prog;
       prog.(0) = Set (7, 0) &&
       prog.(1) = Set (8, 1) &&
       prog.(2) = Set (9, 2) &&
       prog.(3) = Reset 3    &&
       prog.(4) = Reset 4    &&
       prog.(31) = Set (0, 2)
    ),
    true, fprintf_bool;
  ]

let tests_make_list =
  [ "make_list_test_10",
    (fun () ->
       let liste = make_list 10 0 in
       liste = [0;1;2;3;4;5;6;7;8;9]
    ),
    true, fprintf_bool;
    "make_list_test_0",
    (fun () ->
       let liste = make_list 0 0 in
       liste = []
    ),
    true, fprintf_bool;
  ]

let tests_general_compose =
  [ "general_compose_somme_somme_bigger",
    (fun () ->
       let new_prog = general_compose prog_somme_2_args [prog_somme_2_args;prog_bigger] 2 in
       string_of_prog new_prog;
       debug_program new_prog [5;2] false = 8
    ),
    true, fprintf_bool;
    "general_compose_somme_constant_constant",
    (fun () ->
       let new_prog = general_compose prog_somme_2_args [prog_constant_v2 11;prog_constant_v2 12] 0 in
       debug_program new_prog [6] false = 23
    ),
    true, fprintf_bool;
    "general_compose_prog_somme_3_args",
    (fun () ->
       let new_prog = general_compose prog_somme_3_args [prog_constant_v2 11;prog_constant_v2 12;prog_constant_v2 13] 0 in
       debug_program new_prog [] true = 36
    ),
    true, fprintf_bool;
    "general_compose_prog_somme_3_args_mega",
    (fun () ->
       let new_prog = general_compose prog_somme_3_args [compose (prog_constant_v2 99) prog_somme_dizaine_unite;compose (prog_constant_v2 34) prog_somme_dizaine_unite;compose (prog_constant_v2 53) prog_somme_dizaine_unite] 0 in
       debug_program new_prog [] false = 33
    ),
    true, fprintf_bool;
  ]


let tests_if_then_else =
  [ "if_then_else_f",
    (fun () ->
       let test_prog = if_then_else prog_somme_dizaine_unite prog_moins_1 prog_succ in
       debug_program test_prog [0] true = 1
    ),
    true, fprintf_bool;
  ]

let tests_prog_of_expr =
  [ "prog_of_expr_simple",
    (fun () ->
       let prog_expr_test = prog_of_expr (If_then_else (Entier 2, Somme (Entier 11,Entier 12), Somme (Entier 1,Entier 2))) in
       debug_program prog_expr_test [] true = 23
    ),
    true, fprintf_bool;
    "prog_of_expr_complexe",
    (fun () ->
       let prog_expr_test = prog_of_expr (If_then_else (Entier 0, Somme (Entier 11,Entier 12), Somme (Entier 1,Entier 2))) in
       debug_program prog_expr_test [] true = 3
    ),
    true, fprintf_bool;
    "prog_of_expr_very_complexe_case_1",
    (fun () ->
       let prog_expr_test = prog_of_expr (If_then_else (Entier 1, Somme (Entier 11, (If_then_else ((Entier 0),(Entier 11),(Entier 42)))), Somme (Entier 1,Entier 2))) in
       debug_program prog_expr_test [] true = 53
    ),
    true, fprintf_bool;
    "prog_of_expr_very_complexe_case_2",
    (fun () ->
       let prog_expr_test = prog_of_expr (If_then_else (Entier 1, Somme (Entier 11, (If_then_else ((Entier 1),(Entier 11),(Entier 42)))), Somme (Entier 1,Entier 2))) in
       string_of_prog prog_expr_test;
       debug_program prog_expr_test [] true = 22
    ),
    true, fprintf_bool;
    "prog_of_expr_very_complexe_case_3",
    (fun () ->
       let prog_expr_test = prog_of_expr (If_then_else (Entier 0, Somme (Entier 11, (If_then_else ((Entier 0),(Entier 11),(Entier 42)))), Somme (Entier 1,Entier 2))) in
       debug_program prog_expr_test [] true = 3
    ),
    true, fprintf_bool;
  ]

(***************************************************************************)
(***************************************************************************)
(***************************************************************************)
(********************************* MAIN ************************************)
(***************************************************************************)
(***************************************************************************)
(***************************************************************************)

let main () =
  printf "\027[31mTEST RUN_INSTRUCTION\027[0m\n";
  List.iter do_test tests_run_instruction_bool;
  List.iter do_test tests_run_instruction_exn;
  printf "\027[31mTEST STEP_PROGRAM\027[0m\n";
  List.iter do_test tests_step_program_bool;
  List.iter do_test tests_run_instruction_exn;
  printf "\027[31mTEST RUN_FUNCTION\027[0m\n";
  List.iter do_test tests_debug_program_bool;
  printf "\027[31mTEST RHO\027[0m\n";
  List.iter do_test tests_rho;
  printf "\027[31mTEST NORMALIZE\027[0m\n";
  List.iter do_test tests_normalize;
  printf "\027[31mTEST CLEAN_NORMALIZE\027[0m\n";
  List.iter do_test tests_clean_program;
  printf "\027[31mTEST TRANSLATE_JUMP\027[0m\n";
  List.iter do_test tests_translate_jump;
  printf "\027[31mTEST COMPOSE\027[0m\n";
  List.iter do_test tests_compose;
  printf "\027[31mTEST TRANSLATE\027[0m\n";
  List.iter do_test tests_translate;
  printf "\027[31mTEST MAKE_LIST\027[0m\n";
  List.iter do_test tests_make_list;
  printf "\027[31mTEST GENERAL_COMPOSE\027[0m\n";
  List.iter do_test tests_general_compose;
  printf "\027[31mTEST IF_THEN_ELSE\027[0m\n";
  List.iter do_test tests_if_then_else;
  printf "\027[31mTEST PROG_OF_EXPR\027[0m\n";
  List.iter do_test tests_prog_of_expr

let _ = main ()
