type metro = STATION of name
					 | AREA of name * metro
					 | CONNECT of metro * metro
and name = string
;;
(*checkMetro: metro -> bool*)
(*AREA : name * metro -> metro*)
(*connect : metro * metro -> station*)
let compare_str : string*string -> bool =
	fun(s1,s2) ->
		let i = String.length s1
		let c = String.get s2 0
		try String.index s2 c1 with Not_found -> FALSE
		TRUE
;;

let rec checkMetro m =
 match m with
 | AREA (a1,a2) -> 
 	(
 | STATION s -> s
 | CONNECT (m1,m2) ->


