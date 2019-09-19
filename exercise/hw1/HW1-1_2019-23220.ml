type nat = ZERO | SUCC of nat
;;
let rec natadd : nat*nat -> nat =
	fun(a,b) ->
	match a with
	| ZERO -> b
	| SUCC a_pre -> SUCC(natadd(a_pre,b))
;;

let rec natmul : nat*nat -> nat =
	fun(c,d) ->
	match c with
	| ZERO -> ZERO
	| SUCC c_pre -> 
		let nat_mul = natmul(c_pre,d)in
		natadd(nat_mul,d)
;;


