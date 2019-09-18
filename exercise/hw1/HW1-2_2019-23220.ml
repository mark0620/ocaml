type formula = TRUE
						 | FALSE
						 | NOT of formula
						 | ANDALSO of formula * formula
						 | ORELSE of formula * formula
						 | IMPLY of formula * formula
						 | LESS of expr * expr
and expr = NUM of int
					| PLUS of expr * expr
					| MINUS of expr * expr
;;

let rec int_of_expr e =
	match e with
	| NUM i -> i
	| PLUS (e1,e2) -> (int_of_expr e1) + (int_of_expr e2)
	| MINUS (e1,e2) -> (int_of_expr e1) + (int_of_expr e2)
;;
(* eval : formula -> bool *)
let rec eval f =
	match f with
	| TRUE -> true
	| FALSE -> false
	| NOT not_f -> (eval not_f)
	| ANDALSO (f1, f2)-> (eval f1) && (eval f2)
	| ORELSE (f1, f2)-> (eval f1) || (eval f2)
	| IMPLY (f1 ,f2)-> (eval f1) && not(eval f2)
	| LESS (e3, e4)->
		if((int_of_expr e3) < (int_of_expr e4))
		then (eval TRUE) 
		else (eval FALSE) 
;;
