
(** Abstract syntax of MiniML expressions *)

type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Unop of varid * expr                 (* unary operators *)
  | Binop of varid * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
 and varid = string ;;
  
(** Sets of varids *)
module SS = Set.Make(struct
		      type t = varid
		      let compare = String.compare
		    end);;
  
type varidset = SS.t ;;

(** Test to see if two sets have the same elements (for
    testing purposes) *)
let same_vars = SS.equal;;

(** Generate a set of variable names from a list of strings (for
    testing purposes) *)
let vars_of_list = SS.of_list ;;
  
(** Return a set of the variable names free in [exp] *)
let rec free_vars (exp : expr) : varidset =
  match exp with 
  | Num _ | Bool _ -> SS.empty
  | Var x -> SS.singleton x
  | Conditional (x, y, z) -> 
      SS.union (SS.union (free_vars x) (free_vars y)) (free_vars z)
  | Unop (x, y) -> free_vars y
  | Binop (_, y, z) | App (y, z) -> SS.union (free_vars y) (free_vars z)
  | Fun (x, y) -> SS.remove x (free_vars y)
  | Let (x, y, z) -> SS.union (SS.remove x (free_vars z)) (free_vars y) 
  | Letrec (x, y, z) -> 
      SS.union (SS.remove x (free_vars z)) (SS.remove x (free_vars y))
  | Raise | Unassigned -> SS.empty
  ;;

(** Return a fresh variable, constructed with a running counter a la
    gensym. Assumes no variable names use the prefix "var". 
    Should be called with a single unit argument. *)
let new_varname =
  let count = ref 0 in 
  fun () ->
    (count := !count + 1;
    "x" ^ string_of_int !count) 

(** Substitute [repl] for free occurrences of [var_name] in [exp] *)
 (* exp [var_name -> repl] *)
let rec subst (var_name: varid) (repl: expr) (exp: expr) : expr =
  match exp with 
  | Num _ | Bool _ -> exp
  | Var x -> if x = var_name then repl else exp
  | Conditional (x, y, z) -> 
      Conditional(subst var_name repl x, subst var_name repl y, 
        subst var_name repl z)
  | Unop (op, y) -> Unop(op, subst var_name repl y) 
  | Binop (op, y, z) -> Binop(op, subst var_name repl y, subst var_name repl z)
  | App (x, y) -> App(subst var_name repl x, subst var_name repl y)
  | Fun (x, y) -> 
      if x = var_name then Fun (x, y)
      else if not (SS.mem x (free_vars repl)) then
        Fun (x, subst var_name repl y)
      else let z = new_varname () in 
        Fun (z, subst var_name repl (subst x (Var z) y))
  | Let (x, def, body) -> 
      if x = var_name then Let (x, subst x repl def, body)
      else if not (SS.mem x (free_vars repl)) then
        Let (x, subst var_name repl def, subst var_name repl body)
      else let a = new_varname () in 
        Let (a, subst var_name repl def, 
          subst var_name repl (subst x (Var a) body))
  | Letrec (x, def, body) -> 
      if x = var_name then Letrec (x, def, body)
      else if not (SS.mem x (free_vars repl)) then
        Letrec (x, subst var_name repl def, subst var_name repl body)
      else let a = new_varname () in 
        Letrec (a, subst var_name repl def, 
          subst var_name repl (subst x (Var a) body))
  | Raise -> Raise
  | Unassigned -> Unassigned

(** Returns a string representation of the expr *)
let rec exp_to_string (exp: expr) : string =
  match exp with
  | Var x -> "Var(" ^ x ^ ")"
  | Num x -> "Num(" ^ (string_of_int x) ^ ")"
  | Bool x -> "Bool(" ^ (string_of_bool x) ^ ")"
  | Unop (x, y) -> "Unop(" ^ x ^ ", " ^ (exp_to_string y) ^ ")"
  | Binop (x, y, z) -> 
      "Binop(" ^ x ^ ", " ^ (exp_to_string y) ^ ", " ^ (exp_to_string z) ^ ")"
  | Conditional (x, y, z) -> 
      "Conditional(" ^ (exp_to_string x) ^ ", " ^ 
        (exp_to_string y) ^ ", " ^ (exp_to_string z) ^ ")"
  | Fun (x, y) -> "Fun(" ^ x ^ ", " ^ (exp_to_string y) ^ ")"
  | Let (x, y, z) -> 
      "Let(" ^ x ^ ", " ^ (exp_to_string y) ^ ", " ^ (exp_to_string z) ^ ")"      
  | Letrec (x, y, z) -> 
      "Letrec(" ^ x ^ ", " ^ (exp_to_string y) ^ ", " ^ (exp_to_string z) ^ ")"    
  | Raise -> "Raise"
  | Unassigned -> "Unassigned"
  | App (x, y) -> "App(" ^ (exp_to_string x) ^ ", " ^ (exp_to_string y) ^ ")"

