(** Tests for mini-ml
    @author Phillip Yu

    Generates a testing report for the MiniML langauge. 
 *)

open Evaluation ;;
open Expr ;;
open Printf ;;

exception Timeout ;;

type test =
{ label: string;
	content: bool Lazy.t;
	time: int;
	fail_msg: string
} ;;

type status =
 | Passed
 | Failed of string
 | Raised_exn of string
 | Timed_out of int

let sigalrm_handler =
	Sys.Signal_handle (fun _ -> raise Timeout) ;;

let timeout (time : int) (delayed : 'a Lazy.t) : 'a =
	let old_behavior =
		Sys.signal Sys.sigalrm sigalrm_handler in
	let reset_sigalrm () =
		ignore (Unix.alarm 0);
		Sys.set_signal Sys.sigalrm old_behavior in
	ignore (Unix.alarm time) ;
	let res = Lazy.force delayed in
	reset_sigalrm () ; res ;;

let run_test ({label; time; content; fail_msg} : test)
							(continue : string -> status -> unit)
						: unit =
	try
		if timeout time content
		then continue label Passed
		else continue label (Failed fail_msg)
	with
	| Timeout -> continue label (Timed_out time)
	| exn -> continue label (Raised_exn (Printexc.to_string exn))

let present label status =
	match status with
	| Passed -> printf "%s: passed\n" label
	| Failed msg ->
			printf "%s: failed %s\n" label msg
	| Timed_out secs ->
			printf "%s: timed out in %d\n" label secs
	| Raised_exn msg ->
			printf "%s: raised %s\n" label msg ;;

let report (tests: test list) : unit =
	List.iter (fun test -> run_test test present) tests ;;

let test ?(fail_msg="somehow") ?(time=5) label content =
	{label = label;
	content = content;
	fail_msg = fail_msg;
	time = time} ;;

let eval_test eval eval_string env =
	[
	test (eval_string ^ " let num fun binop app1")
		(lazy (let test1 = Let("x", Num 3, Let("double", 
												Fun("x", Binop("+", Var("x"), Var("x"))), 
		              			App(Var("double"), Num(4)))) in
           eval test1 env = Env.Val (Num 8))) ;
	test (eval_string ^ " letrec cond")
		(lazy (let test1 = Letrec("f", Fun("x", Conditional(
												Binop("=", Var "x", Num(0)), Num(1), 
					              Binop("*", Var("x"), App(Var("f"), 
					              Binop("-", Var "x", Num(1)))))), 
					              App(Var("f"), Num(4))) in
           eval test1 env = Env.Val (Num 24))) ;
	test (eval_string ^ " unop ")
		(lazy (let test1 = Letrec("test", Fun("v", Conditional(
												Binop("=", Var("v"), Num(1)), Unop("~", Var("v")), 
					            	Binop("*", Var("v"), App(Var("test"), 
					            	Binop("-", Var("v"), Num(1)))))), 
					            	App(Var("test"), Num(6))) in
           eval test1 env = Env.Val (Num (-720)))) ;
	test (eval_string ^ "; SHOULD RAISE EXCEPTION")
		(lazy (let test1 = Let("x", Num(5), App(Var("x"), Num(6))) in
           eval test1 env <> Env.Val (Num 0))) ;
	test (eval_string ^ ": pass if lexically scoped, fail if dynamically")
		(lazy (let test1 = Let("x", Num(1), Let("f", 
												Fun("y", Binop("+", Var("x"), Var("y"))), 
					             	Let("x", Num(2), App(Var("f"), Num(3))))) in
           eval test1 env = Env.Val (Num 4))) ;
	]

let tests =
	[ 
	test "exp_to_string 1" 
		(lazy (let test = Let("f", Fun("x", Var("x")), 
											App(App(Var("f"), Var("f")), Num(3))) in
          exp_to_string test = 
         		"Let(f, Fun(x, Var(x)), App(App(Var(f), Var(f)), Num(3)))")) ;
	test "exp_to_string 2" 
		(lazy (let test = Conditional(Bool(true), Raise, 
											Binop("-", Num(3), Num(2))) in
          exp_to_string test = 
          	"Conditional(Bool(true), Raise, Binop(-, Num(3), Num(2)))")) ;
	test "exp_to_string 3" 
		(lazy (let test = Letrec("g", Unop("~-", Var("x")), Unassigned) in
          exp_to_string test = "Letrec(g, Unop(~-, Var(x)), Unassigned)")) ;
	test "free_vars 1" 
		(lazy (let test1 = Fun("y", App(Var("f"), Binop("+", Var("x"), Var("y")))) in
          let test2 = vars_of_list ["x"; "f"] in
          same_vars (free_vars test1) test2)) ;
	test "free_vars 2" 
		(lazy (let test1 = Conditional(Bool(false), Var("x"), Num(3)) in
          let test2 = vars_of_list ["x"] in
          same_vars (free_vars test1) test2)) ;
	test "free_vars 3" 
		(lazy (let test1 = Let("x", Num(3), Let("double", 
												Fun("x", Binop("+", Var("x"), Var("x"))), 
					            	App(Var("double"), Num(4)))) in
          let test2 = vars_of_list [] in
        	same_vars (free_vars test1) test2)) ;
	test "free_vars 4" 
		(lazy (let test1 = Let("x", Num(3), Let("y", Var("x"), 
												App(App(Var("f"), Var("x")), Var("y")))) in
          let test2 = vars_of_list ["f"] in
        	same_vars (free_vars test1) test2)) ;
	test "free_vars 5 let rec" 
		(lazy (let test1 = Letrec("x", Fun("y", Var("x")), Var("x")) in
          let test2 = vars_of_list [] in
        	same_vars (free_vars test1) test2)) ;
	test "free_vars 6 let rec 2" 
		(lazy (let test1 = Let("x", Num(5), Letrec("y", 
												Binop("*", Var("x"), Var("y")), 
            						Fun("y", Binop("+", Binop("+", Var("x"), 
            						Var("z")), Var("y"))))) in
          let test2 = vars_of_list ["z"] in
        	same_vars (free_vars test1) test2)) ;
	test "subst num" 
		(lazy (let test1 = Var "x" in
          let test2 = Num 12 in
        	subst "x" (Num 12) test1 = test2)) ;
	test "subst fun1 binop var" 
		(lazy (let test1 = Fun("x", Binop("+", Var "x", Var "x")) in
          let test2 = test1 in
        	subst "x" (Num 12) test1 = test2)) ;
	test "subst let" 
		(lazy (let test1 = Let("x", Binop("*", Var("y"), Var("y")), 
												Binop("+", Var("x"), Var("x"))) in
          let test2 = test1 in
        	subst "x" (Num 3) test1 = test2)) ;
	test "subst let 2" 
		(lazy (let test1 = Let("x", Binop("*", Var("x"), Var("x")), 
												Binop("+", Var("x"), Var("x"))) in
          let subber = Num 3 in
          let test2 = Let("x", Binop("*", subber, subber), 
          							Binop("+", Var("x"), Var("x"))) in
        	subst "x" subber test1 = test2)) ;     
 	test "subst fun3" 
		(lazy (let test1 = Fun("y", Var("x")) in
          let subber = Var "y" in
          let test2 = Fun("y", subber) in
        	subst "x" subber test1 <> test2)) ;
 	test "subst bool cond unop app fun2" 
		(lazy (let test1 = Conditional(Bool(true), Unop("~", Var("x")), App(Fun("y", 
            						Binop("*", Binop("*", Var("y"), Var("x")), 
            						Num(2))), Num(4))) in
          let subber = Num 3 in
          let test2 = Conditional(Bool(true), Unop("~", subber), App(Fun("y", 
            					Binop("*", Binop("*", Var("y"), subber), Num(2))), Num(4))) in
        	subst "x" subber test1 = test2)) ;
  test "subst letrec1" 
		(lazy (let test1 = Let("y", Num(5), Letrec("x", 
												Binop("*", Var("y"), Var("x")), Var("x"))) in
          let subber = Num 3 in
          let test2 = test1 in
        	subst "x" subber test1 = test2)) ;             			
	test "close"
		(lazy (let env = Env.create () in
					let exp = Num 6 in
					Env.close exp env = Env.Closure (exp, env)
					)) ;
	test "remaining part6 functions ; SHOULD RAISE EXCEPTION"
		(lazy (let env = Env.create () in
					 let env1 = Env.extend env "foo" (ref (Env.Val (Num 5))) in
					 let env2 = Env.extend env1 "bar" (ref (Env.Val (Num 7))) in
					 let env3 = Env.extend env2 "foo" (ref (Env.Val (Num 9))) in
					 assert(Env.env_to_string env1 = "(foo, Num(5)); ");
					 assert(Env.env_to_string env2 = "(foo, Num(5)); (bar, Num(7)); ");
					 assert(Env.env_to_string env3 = "(foo, Num(9)); (bar, Num(7)); ");
					 assert(Env.lookup env3 "bar" = Env.Val (Num 7));
					 Env.lookup env3 "baz" = Env.Val (Num 7)
					)) ;
	test "eval_s; SHOULD RAISE EXCEPTION"
		(lazy (let test1 = Var "x" in
          eval_s test1 "" <> Env.Val (Num 0))) ;
 	test "eval_d val"
		(lazy (let env = Env.create () in
					 let env1 = Env.extend env "foo" (ref (Env.Val (Num 5))) in
					 eval_d (Var "foo") env1 = Env.Val (Num 5))) ;
	] @ eval_test eval_s "eval_s" ""
		@ eval_test eval_d "eval_d" (Env.create())
		@ eval_test eval_l "eval_l" (Env.create())
;;

report tests ;;

