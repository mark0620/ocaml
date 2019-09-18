type nat = ZERO | SUCC of nat
let one = SUCC(ZERO)
let two = SUCC(SUCC(ZERO))
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
	| one -> d
	| SUCC c_pre -> natadd(natmul(c_pre,d),d)
;;
