type expr = NUM of int
					| PLUS of expr * expr
					| MINUS of expr * expr
					| MULT of expr * expr
					| DIVIDE of expr * expr
					| MAX of expr list
;;

let rec int_max : int*int -> int =
	fun(a,b) ->
		if(a>b) then a else b
;;
(* eval : expr -> int *)
let rec eval (f: expr) : int =
	let rec max_list (l: expr list) : int =
		match l with
		| [] -> 0
		(*| head::[] -> eval head*)
		| head::tail -> int_max(eval head,max_list tail)
	in
	match f with
	| NUM i -> i
	| PLUS (e1,e2) -> (eval e1) + (eval e2)
	| MINUS (e1,e2) -> (eval e1) - (eval e2)
	| MULT (e1,e2) -> (eval e1) * (eval e2)
	| DIVIDE (e1,e2) -> (eval e1) / (eval e2)
	| MAX l -> max_list l

;;
