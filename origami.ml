(* ========== ORIGAMI ========== *)
(* Anita Śledź *)
(* Review - Michał Niedziółka *)


type point = float * float;;

type kartka = point -> int ;;

(**Funkcje pomocnicze**)


(*Funkcje do ładnego porównywania iloczynu wektorowego*)
let eps = 0.00000000001 ;;
let abs x = if x < 0. then -.x else x;;
let cmp a b = abs (a -. b) < eps;;


(* Iloczyn wektorowy sprawdza, po której stronie wektoru PR jest Q -> 0 na linii, 1 -> po prawej, -1 -> po lewej*)
let iloczyn_wektorowy (p1, p2) (q1, q2) (r1, r2) =
  let wynik = ((r1 -. p1) *. (q2 -. p2)) -. ((r2 -. p2) *. (q1 -. p1))
  in
  if cmp wynik 0. then 0
  else if wynik > 0. then 1
  else -1
;;

(* Zwraca współczynniki prostej przechodzącej przez punkt a i b *)
let prosta (ax, ay) (bx, by) =
  let a = (ay -. by) /. (ax -. bx)
  and b = ay -. ( ax *. ((ay -. by) /. (ax -. bx)) )
  in (a, b);;

(* Odbicie punktu x y względem prostej przechodzącej przez punkty p1 p2 i q1 q2 *)
let odbicie (p1, p2) (q1, q2) (x, y) =
  if p1 = q1 then ( (p1 +. p1 -. x), y )
  else
    if p2 = q2 then (x, (p2 +. p2 -. y))
    else
    let (a, b) = prosta (p1, p2) (q1, q2) in
    let xs = ((a *. (y -. b) +. x)) /. (a *. a +. 1.0) in
    let ys = a *. xs +. b in
    let xr = 2.0 *. xs -. x and yr = 2.0 *. ys -. y in
    (xr, yr)
;;

(**Funkcje zasadnicze**)

let prostokat (x1, y1) (x2, y2) (px, py) =
  if (px <= x2 && px >= x1 && py <= y2 && py >= y1) then 1
  else 0 ;;

let kolko (x1, y1) r (px, py) =
  if ((px -. x1) *. (px -. x1) +. (py -. y1) *. (py -. y1)) <= r *. r then 1
  else 0;;

let zloz p r k q =
  if iloczyn_wektorowy p r q = 0 then k q
  else
    if iloczyn_wektorowy p r q = 1 then 0
    else let q2 = odbicie p r q in k q + k q2;;

let skladaj l k =
    List.fold_left (fun a (x, y) -> zloz x y a ) k l;;
