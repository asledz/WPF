(* Sortowanie topoplogiczne *)
(* Anita Śledź 406384 *)
(* Review - Jakub Organa *)

open pMap;;

type state = None | Visited | Procesing ;;

exception Cykliczne;;

let do_nothing = () ;;

let topol l = 
	let return = ref []
	and graf = ref empty

	(* Tworzenie grafu *)
	in List.iter ( function (v, nb) -> graf := add v (None, nb) !graf ) l;
	List.iter( function (v, nb) -> 
		List.iter (function a -> 
			let _ = (try (find a !graf) with Not_found -> graf := add a (None,[]) !graf ; find a !graf) in do_nothing
		) nb
	) l;

	(* Przejście po grafie dfsem *)
	let rec dfs v = 
		let (status, nb) = find v !graf
		in
		match status with
		| Visited -> do_nothing
		| _ -> (
			graf := add v (Procesing, nb) !graf;
			List.iter (
				function w -> 
					let (status2, _) = find w !graf
					in (match status2 with 
					| None -> dfs w 
					| Procesing -> raise Cykliczne
					| _ -> do_nothing
					)
			) nb ;
			graf := add v (Visited, nb) !graf;
			return := v::(!return)
		)
	in
	List.iter (function(v,_) -> dfs v) l;
	!return
;;
