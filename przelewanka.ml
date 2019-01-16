(** PRZELEWANKA **)
(* Anita Śledź *)
(* Review - Stanisław Strzelecki *)


let przelewanka tab = 
	let n = Array.length tab in
	let mapa = Hashtbl.create n
	and q = Queue.create () in

	Queue.add (Array.make n 0) q;
	Hashtbl.add mapa (Array.make n 0) 0;

	(* my_state - końcowy stan który chcemy otrzymać *)
	let my_state = (Array.make n 0) in
	for i = 0 to n-1 do my_state.(i) <- snd (tab.(i)) done;

	(* BFS po wszystkich stanach *)
	while (Queue.is_empty q = false) 
	do
		(* state - stan aktualnie rozpatrywany z góry kolejki *)
		let state = Queue.take q in
		let dis = Hashtbl.find mapa state in

		(* funkcja przelej tworzy tablicę ze stanem po przelaniu wody ze szklanki a do szklanki b *)
		let przelej a b =
			let nb = state
			and poj = fst (tab.(b)) in
			if (a<>b && state.(a) > 0) then
			(
				if (state.(a) + state.(b)) <= poj then ( nb.(a) <- 0; nb.(b) <- (state.(a) + state.(b)); nb )
				else (nb.(b) <- poj; nb.(a) <- (state.(a) - (poj-state.(b))); nb )
			)
			else nb

		(* funkcja uzupełnij tworzy tablicę ze stanem po uzupełnieniu szklanki a do pełna *)
		and uzupelnij a = 
			let nb = state
			and poj = fst (tab.(a)) in
			nb.(a) <- poj; nb
		
		(* funkcja zeruj tworzy tablicę ze stanem po wylaniu wody ze szklanki a*)
		and zeruj a = 
			let nb = state in
			nb.(a) <- 0; nb

		(* funkcja dodaj dodaje nowy stan t do mapy i do kolejki *)
		and dodaj t = 
			if Hashtbl.mem mapa t then () else (Hashtbl.add mapa t (dis+1) ; Queue.add t q)
		in

		for i = 0 to (n-1) do
			for j = 0 to (n-1) do
				dodaj (przelej i j)
			done
		done;

		for i = 0 to (n-1) do
			dodaj (uzupelnij i);
			dodaj (zeruj i)
		done;


	done;

	if (Hashtbl.mem mapa my_state) then (Hashtbl.find mapa my_state) else -1

;;
