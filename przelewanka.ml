(** Zadanie przelewanka **)
(* Anita Śledź 406384 *)
(* Review - Stanisław Strzelecki *)


let przelewanka tab = 
	let n = Array.length tab in
	let mapa = Hashtbl.create n
	and q = Queue.create () in

	Queue.add (Array.make n 0) q;
	Hashtbl.add mapa (Array.make n 0) 0;

	(* Szukany stan *)
	let final_state = (Array.make n 0) in
	for i = 0 to n-1 do final_state.(i) <- (snd (tab.(i))) done;

	(* BFS po wszystkich stanach *)
	while (Queue.is_empty q = false)
	do
		let state = Queue.take q in
		let dis = Hashtbl.find mapa state in

		for i = 0 to (n-1) do
			for j = 0 to (n-1) do 
				let nb = Array.copy state 
				and cap = fst (tab.(j)) in

				if (i <> j && state.(i) > 0) then (
					if (state.(i) + state.(j)) <= cap then (
						nb.(i) <- 0;
						nb.(j) <- (state.(i) + state.(j) )
					)
					else (
						nb.(i) <- (state.(i) - (cap - state.(j)));
						nb.(j) <- cap
					)
				)
				else ();

				if Hashtbl.mem mapa nb then ()
				else (Hashtbl.add mapa nb (dis+1); Queue.add nb q)

			done
		done;

		for i = 0 to (n-1) do
			let nb = Array.copy state
			and nb2 = Array.copy state
			and cap = fst (tab.(i)) in
			nb.(i) <- cap;
			if Hashtbl.mem mapa nb then ()
			else (Hashtbl.add mapa nb (dis+1); Queue.add nb q);
			nb2.(i) <- 0;
			if Hashtbl.mem mapa nb2 then ()
			else (Hashtbl.add mapa nb2 (dis+1); Queue.add nb2 q)
		done;

		if state = final_state then Queue.clear q else ();

	done;

	if (Hashtbl.mem mapa final_state) then (Hashtbl.find mapa final_state) else -1


;;
