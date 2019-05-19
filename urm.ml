open Printf

(*
   Type somme décrivant les instructions de notre URM
*)
type instruction =
  | Reset of int
  | Incr of int
  | Set of int * int
  | Jump of int * int * int

(*
   Type somme décrivant des constantes  entières (positives) et des sommes
   d’expression
*)
type expression =
  | Entier of int
  | Somme of expression * expression
  | If_then_else of expression * expression * expression


(*
   Type décrivant un programme
*)
type programme = instruction array

(*
   Quelques exceptions
*)
exception Memory_Exhausted
exception Segmentation_Fault
exception Resources_Exhausted

let max_registers = 20
let max_steps = 2000


(***************************************************************************)
(***************************************************************************)
(***************************************************************************)
(************************* QUELQUES PROGRAMMES *****************************)
(***************************************************************************)
(***************************************************************************)
(***************************************************************************)


(* [prog_succ] renvoie le successeur du premier registre
*)
let prog_succ = [| Incr 0 |]

(* [prog_somme_2_args] renvoie la somme des deux premiers registres
*)
let prog_somme_2_args =
  [| Jump (1, 2, 5);
     Incr 0;
     Incr 2;
     Jump (1, 2, 5);
     Jump (1, 1, 1) |]

(* [prog_somme_3_args] renvoie la somme des trois premiers registres
*)
let prog_somme_3_args =
  [| Jump (1, 3, 3);
     Incr 0;
     Incr 3;
     Jump (2, 4, 9);
     Incr 0;
     Incr 4;
     Jump (1, 3, 3);
     Jump (2, 4, 0);
     Jump (1, 1, 0) |]

(* [prog_somme_modify] renvoie la somme des deux premiers registres
*)
let prog_somme_modify =
  [| Jump (1, 2, 5);
     Incr 0;
     Incr 2;
     Jump (1, 2, 10);
     Jump (1, 1, 1) |]

(* [prog_constant] renvoie l'entier passé en paramètre
*)
let prog_constant =
  [| Jump (0, 1, 4);
     Incr 1;
     Jump (0, 1, 4);
     Jump (1, 1, 1) |]

(* [prog_constant_v2] renvoie l'entier passé en paramètre
*)
let prog_constant_v2 nb = Array.make nb (Incr 0)

(* [prog_bigger] renvoie 1 si le premier registre est plus grand que le
   deuxième, 0 sinon
*)
let prog_bigger =
  [| Jump (1, 3, 7);
     Jump (0, 2, 10);
     Incr 2;
     Incr 3;
     Jump (1, 3, 7);
     Jump (0, 2, 10);
     Jump (1, 1, 2);
     Reset 0;
     Incr 0;
     Jump (1, 1, 11);
     Reset 0 |]

(* [prog_moins_1] renvoie le contenu du premier registre - 1
*)
let prog_moins_1 =
  [| Jump (0, 1, 5);
     Incr 1;
     Jump (0, 1, 5);
     Incr 2;
     Jump (0, 0, 1);
     Set (2, 0) |]

(* [prog_division_2] renvoie le contenu du premier registre divisé par deux
*)
let prog_division_2 =
  [| Jump (1, 0, 5);
     Incr 1;
     Incr 1;
     Incr 2;
     Jump (0, 0, 0);
     Set (2, 0) |]

(* [prog_test_0] renvoie O si le premier registre contient 0, 1 sinon
*)
let prog_test_0 =
  [| Reset 1;
     Jump (1,0,47);
     Reset 0;
     Incr 0 |]

(* [prog_somme_dizaine_unite] renvoie la somme des chiffres du nombre (dizaine+unité)
*)
let prog_somme_dizaine_unite =
  [| Incr 4;
     Incr 4;
     Incr 4;
     Incr 4;
     Incr 4;
     Incr 4;
     Incr 4;
     Incr 4;
     Incr 4;
     Incr 4;
     Jump (0, 1, 19);
     Incr 1;
     Incr 2;
     Jump (2, 4, 16);
     Jump (0, 1, 19);
     Jump (0, 0, 10);
     Reset 2;
     Incr 3;
     Jump (0, 0, 10);
     Reset 0;
     Jump (0, 3, 25);
     Incr 0;
     Incr 2;
     Jump (0, 3, 25);
     Jump (0, 0, 21);
     Set (2, 0) |]


(***************************************************************************)
(***************************************************************************)
(***************************************************************************)
(*************************** FONCTIONS DU TP *******************************)
(***************************************************************************)
(***************************************************************************)
(***************************************************************************)

(* [string_of_prog r_prog] affiche le programmes

   @requires  r_prog est le pogramme
   @ensures   le résultat est affiché sur la sortie standard
*)
let rec string_of_prog prog =
  Array.iteri (fun i ins ->
      match ins with
      | Reset r -> printf "%d : Reset %d\n" i r
      | Incr r -> printf "%d : Incr  %d\n" i r
      | Set (r1, r2) -> printf "%d : Set %d, %d\n" i r1 r2
      | Jump (r1, r2, s) -> printf "%d : Jump %d, %d, %d\n" i r1 r2 s) prog

(* [print_registers r_tab r_max] affiche le tableau des registres modifiés
   et/ou accédés

   @requires  r_tab est un ensemble de registres, r_max l'indice du dernier
              registre modifié et/ou accéder
   @ensures   le résultat est affiché sur la sortie standard
*)
let rec print_registers r_tab r_max =
  print_string "[ " ;
  Array.iteri (fun i r ->
      if i <= r_max then (
        print_int r ;
        print_string " ")) r_tab;
  print_endline "]"

(* [run_instruction r_tab ins i] exécute l'instruction passée en paramètre et
   renvoie l'indice de la prochaine instruction à exécuter

   @requires  r_tab est un ensemble de registres, ins une instruction et i son
              indice
   @ensures   le résultat est l'indice de la prochaine instruction à exécuter
*)
let run_instruction r_tab ins i =
  (*print_registers r_tab;*)
  match ins with
  | Reset r ->
    (*print_endline "reset";*)
    if r > max_registers - 1
    then raise Memory_Exhausted
    else r_tab.(r) <- 0 ;
    i+1
  | Incr r ->
    (*print_endline "incr";*)
    if r > max_registers - 1
    then raise Memory_Exhausted
    else r_tab.(r) <- r_tab.(r) + 1 ;
    i+1
  | Set (r1, r2) ->
    (*print_endline "set";*)
    if r1 > max_registers - 1 || r2 > max_registers - 1
    then raise Memory_Exhausted
    else r_tab.(r2) <- r_tab.(r1) ;
    i+1
  | Jump (r1, r2, s) ->
    (*print_endline "jump";*)
    if r1 > max_registers - 1 || r2 > max_registers - 1
    then raise Segmentation_Fault
    else if r_tab.(r1) = r_tab.(r2) then s else i+1

(* [step_program r_tab prog max_steps] exécute le programme passé en paramètre
   et renvoie les registres après exécution

   @requires  r_tab est un ensemble de registres, prog un programme et
              max_steps le nombre maximum d'étapes à exécuter
   @ensures   le résultat est l'ensemble des registres mis à jour
*)
let step_program r_tab prog max_steps =
  let first_ins = prog.(0) in
  let first_i = 0 in
  let current_step = 0 in
  let n = Array.length prog in
  let rec step_program_aux r_tab prog current_step ins i =
    (*print_endline "---";*)
    if current_step <= max_steps then
      begin
        let next_i = run_instruction r_tab ins i in
        if next_i < n then
          let current_step = current_step + 1 in
          let next_inst = prog.(next_i) in
          step_program_aux r_tab prog current_step next_inst next_i
        else r_tab
      end
    else if i < n then raise Resources_Exhausted else r_tab
  in
  step_program_aux r_tab prog current_step first_ins first_i

(* [rho prog] renvoie l'indice du plus grand registre modifié par le
   programme

   @requires  prog un programme
   @ensures   le résultat est l'indice en question, un entier
*)
let rho prog = Array.fold_left (fun reg_max current_ins ->
    match current_ins with
    | Reset r
    | Incr r -> if r > reg_max then r else reg_max
    | Jump (r1, r2, _)
    | Set (r1, r2) ->
      if r1 > reg_max || r2 > reg_max
      then if r2 > r1 then r2 else r1
      else reg_max) 0 prog

(* [clean_program prog] renvoie le programme prog "nettoyé"

   @requires  prog un programme
   @ensures   le résultat est un programme "nettoyé"
*)
let clean_program prog =
  let r_max = rho prog in
  let prog_end = Array.make (r_max) (Reset 0) in
  let rec clean_program_aux current_r =
    if current_r >= 1 then
      begin
        prog_end.(current_r-1) <- Reset current_r;
        clean_program_aux (current_r-1)
      end
  in
  clean_program_aux r_max;
  Array.append prog prog_end

(* [normalize prog] renvoie le programme prog normalisé

   @requires  prog un programme
   @ensures   le résultat est un programme
*)
let normalize prog =
  let n = Array.length prog in
  let prog_copy = Array.copy prog in
  let rec normalize_aux prog prog_copy cur max =
    if cur = max then prog_copy else
      match prog.(cur) with
      | Jump (r1, r2, s) ->
        if (s > n) then
          prog_copy.(cur) <- Jump (r1,r2,max);
        normalize_aux prog prog_copy (cur + 1) max
      | _ -> normalize_aux prog prog_copy (cur + 1) max
  in normalize_aux prog prog_copy 0 n

(* [debug_program prog args_tab debug] exécute le programme passée en
   paramètre avec ses arguments et renvoie le résultat obtenu

   @requires  prog un programme, args_tab le tableau des arguments et debug
              un texte de debug
   @ensures   le résultat est la sortie de la fonction
*)
let debug_program prog args_tab debug =
  let norm_prog = normalize prog in
  let r_tab = Array.make max_registers 0 in
  List.iteri (fun i e -> r_tab.(i) <- e) args_tab;
  let _ =
    if debug then
      (print_string "Avant : ";print_registers r_tab (rho norm_prog))
  in
  let _ = step_program r_tab norm_prog max_steps in
  let _ =
    if debug then
      (print_string "Après : ";print_registers r_tab (rho norm_prog))
  in
  r_tab.(0)


(* [translate_jump prog prog_copy cur max n_tran] renvoie le programme prog
   ou tout les jump sont translatés de n_trans

   @requires  prog un programme, prog_copy sa copie, cur le curseur pour
              circuler dans le programme, max la taille du programme,
              n_trans la taille de la translation
   @ensures   le programme translaté
*)
let rec translate_jump prog prog_copy cur max n_tran=
  if cur = max then prog_copy else
    match prog.(cur) with
    | Jump (r1, r2, i)->
      prog_copy.(cur) <- Jump (r1, r2, i+n_tran);
      translate_jump prog prog_copy (cur + 1) max n_tran
    | _ -> translate_jump prog prog_copy (cur + 1) max n_tran

(* [compose prog1 prog2] renvoie la concaténation des deux programmes prog1 et
   prog2

   @requires  prog1 et prog2 des programmes
   @ensures   un programme, la concaténation de prog1 et prog2
*)
let compose prog1 prog2 =
  let prog1_copy = Array.copy prog1 in
  let n1 = Array.length prog1_copy in
  let n2 = Array.length prog2 in
  let prog2_copy = Array.copy prog2 in
  let new_prog_2 = translate_jump prog2 prog2_copy 0 n2 n1 in
  Array.append prog1_copy new_prog_2

(* [translate prog r_list_init r_end] renvoie le translaté du programme prog

   @requires  prog le programme à translater, r_list_init la liste des
              registres, r_end le registre où placer le résultat
   @ensures   un programme
*)
let translate prog r_list_init r_end =
  let norm_prog = normalize prog in
  let n = List.length r_list_init in
  let prog_trans = Array.make n (Reset 0) in
  List.iteri (
    fun i e -> prog_trans.(i) <- (Set (e, i))) r_list_init ;
  let r_max = rho norm_prog in
  let prog_init_0 = Array.make (r_max - n + 1) (Reset 0) in
  Array.iteri (
    fun i _ -> prog_init_0.(i) <- Reset (i+n)) prog_init_0;
  let prog_copy = Array.copy norm_prog in
  let prog_n = Array.length norm_prog in
  let new_prog = translate_jump norm_prog prog_copy 0 prog_n (r_max+1) in
  let prog_end = [| Set (0, r_end) |] in
  Array.append prog_trans (Array.append prog_init_0 (Array.append new_prog prog_end))

(* [make_list k current] renvoie le liste des nombre de current à k-1

   @requires  k et current deux entiers
   @ensures   une liste de k - current entiers
*)
let rec make_list k current =
  if current < k then current::(make_list k (current+1)) else []

(* [general_compose prog_f prog_gi_vect k] renvoie un programme qui correspond
   à la composition généralisée de n fonction gi à k arguments par une fonction
   f

   @requires  prog_f le programme associé à la fonction f, prog_gi_vect le
              vecteur des programmes associés aux fonctions gi et k le nombre
              d'arguments
   @ensures   un programme
*)
let general_compose prog_f prog_gi_vect k =
  let norm_prog_f = normalize prog_f in
  let norm_prog_gi_vect = List.map normalize prog_gi_vect in
  let r_max_gi =
    List.fold_left (fun acc p -> max (rho p) acc) 0 norm_prog_gi_vect in
  let r_max_f = rho norm_prog_f in
  let n = List.length norm_prog_gi_vect in
  (********* Étape 1 ********)
  let n_libre = max r_max_gi (max r_max_f (max n k)) in
  let k_list = make_list (n_libre + 1 + k) (n_libre + 1) in
  let prog_save_args = Array.make k (Reset 0) in
  (********* Étape 2 ********)
  Array.iteri (
    fun i e -> prog_save_args.(i) <- (Set (i, n_libre + i + 1))) prog_save_args;
  (********* Étape 3 ********)
  let translated_gi_list =
    List.mapi (
      fun i g ->
        translate g k_list (i+n_libre+k+1)) norm_prog_gi_vect
  in
  let translated_gi = List.fold_left (compose) prog_save_args translated_gi_list
  in
  let new_k_list = make_list (n_libre+k+n+1) (n_libre+k+1) in
  (********* Étape 4 ********)
  let translated_f = translate norm_prog_f new_k_list 0 in
  compose translated_gi translated_f

(* [if_then_else progt progv progf] renvoie un programme conditionnel selon
   les règles énoncées

   @requires  progt, progv et progf trois programmes
   @ensures   un programme
*)
let if_then_else progt progv progf =
  let new_progt = clean_program progt in
  let new_progv = clean_program progv in
  let n_v = Array.length new_progv in
  let n_f = Array.length progf in
  let expr_pivot_1 = [| Reset 1;Jump (0,1,n_v+4);Reset 0|] in
  let expr_pivot_2 = [| Jump (1,1,n_f+1) |] in
  compose new_progt @@ compose expr_pivot_1 @@ compose new_progv @@ compose expr_pivot_2 progf

(* [prog_of_expr expr] renvoie un programme associé à expr

   @requires  expr une expression
   @ensures   un programme
*)
let rec prog_of_expr expr =
  match expr with
  | Entier e -> prog_constant_v2 e
  | Somme (a,b) -> general_compose prog_somme_2_args [prog_of_expr a;prog_of_expr b] 0
  | If_then_else (a,b,c) -> if_then_else (prog_of_expr a) (prog_of_expr b) (prog_of_expr c)