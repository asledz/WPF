(** Zadanie przelewanka **)
(* Anita Śledź 406384 *)
(* Review - Stanisław Strzelecki *)

(* Zwraca nwd dwóch liczb lub 0 gdy a = b = 0*)
let rec gcd a b =
	if b = 0 then a else gcd b (a mod b);;

(* Liczy liczbę ruchów potrzebną do uzyskania stanu opisanego
w tablicy za pomocą szklanek o podanych pojemnościach*)
let przelewanka tab =
	let n = Array.length tab 
	and mapa = Hashtbl.create 10 
	and q = Queue.create () in

	Queue.add (Array.make n 0) q;
	Hashtbl.add mapa (Array.make n 0) 0;


    (* Szukany stan *)
	let final_state = (Array.make n 0)
	and found = ref false

	(* Sprawdzam warunki konieczne, by dało się przelać wodę *)
	and nwd = ref 1 in
	if (n > 0) then nwd := (fst (tab.(0))) else nwd := 1;
 	let is_full = ref false 
	and is_div = ref true in
	
	(* Tworzę tablicę state, iteruję się po tablicy by sprawdzić czy wynik osiągalny *)
	for i = 0 to (n - 1) do
		final_state.(i) <- (snd (tab.(i)));
		nwd := gcd (fst (tab.(i))) !nwd;
		if (snd (tab.(i)) = 0 || fst (tab.(i)) = snd (tab.(i))) then 
			is_full := true 
	done;

	for i = 0 to n-1 do
		if (snd (tab.(i)) != 0) then (
			if (((snd (tab.(i))) mod !nwd) = 0) then ()
			else is_div := false
		) 
	done;
	
	(* Jeśli jakikolwiek wynik końcowy nie dzieli się przez NWD pojemności 
	lub nie ma żadnej kończącej się na stanie pełny/pusty, nie da się osiągnąć wyniku*)
	if (!is_div = false || !is_full = false) then (
		if n = 0 then 0
		else -1
	)

	else begin
		
	(* BFS po stanach *)
	while (Queue.is_empty q = false) do
		let state = Queue.take q in
		let dist = ((Hashtbl.find mapa state) + 1) in
		
		(* Dolewanie i wylewanie wody ze szklanki itej *)
		for i = 0 to (n-1) do
			let fill = Array.copy state
			and spill = Array.copy state 
			and cap = fst (tab.(i)) in
			fill.(i) <- cap;
			spill.(i) <- 0;
			if not (Hashtbl.mem mapa spill) then ( 
				if(spill = final_state) then (found := true); 
				Hashtbl.add mapa spill (dist); Queue.add spill q
			);
			if not (Hashtbl.mem mapa fill) then ( 
				if(fill = final_state) then (found := true); 
				Hashtbl.add mapa fill (dist); Queue.add fill q
			);
		done;
		
		(* Przelewanie wody ze szklanki itej do szklanki jtej *)
		for i = 0 to (n-1) do
			for j = 0 to (n-1) do
				let pour = Array.copy state
				and cap = fst (tab.(j)) in
					if (i<>j && state.(i) > 0) then (
					if (cap >= state.(i) + state.(j)) then (
						pour.(j) <- state.(i) + state.(j);
						pour.(i) <- 0;
					)
					else (
						pour.(j) <- cap;
						pour.(i) <- state.(i) - (cap - state.(j));
					) 
				); 
				if not (Hashtbl.mem mapa pour) then ( 
					if(pour = final_state) then (found := true);
					Hashtbl.add mapa pour (dist); 
					Queue.add pour q
				);

			done;
		done;

		if (!found = true || final_state = state) then Queue.clear q;
	done;
	if (Hashtbl.mem mapa final_state) then (Hashtbl.find mapa final_state) else -1	
	end

;;
