(* Zadanie Arytmetyka przybliżonych wartości *)
(* Anita Śledź 406384 *)
(* Code-review : Gerard Dróżdż *)


open List;;

type wartosc =
  Spojny of (float * float)
  | Niespojny of (float * float) ;;

(*
  Spójnym nazywam przedział w formie <a,b>
  Niespójnym nazywam przedział w formie <-inf,a>u<b,inf>
*)

(*============ KONSTRUKTORY ============*)


let wartosc_dokladnosc x p =
  let blad = abs_float( x *. p /. 100.0 )
  in Spojny((x -. blad , x +. blad));;

let wartosc_od_do x y =
  Spojny( x , y );;

let wartosc_dokladna x =
  Spojny ( x, x );;

(*============ SELEKTORY ============*)

(* Funkcja sprawdzająca, czy w przedziale x znajduje się y - zawsze float *)
let in_wartosc x y =
  match x with
  | Niespojny(lewy, prawy) -> y <= lewy || prawy <= y
  | Spojny(lewy, prawy) -> lewy <= y && y <= prawy;;

(* Funkcja znajdująca najmniejszą wartość przedziału x *)
let min_wartosc x =
  match x with
  | Spojny( lewy , _ ) -> lewy
  | Niespojny( _ , _ ) -> neg_infinity;;

(* Funkcja znajdująca największą wartość przedziału x*)
let max_wartosc x =
  match x with
  | Spojny( _ , prawy ) -> prawy
  | Niespojny ( _ , _ ) -> infinity ;;

(* Funkcja znajdująca średni element przedziału x *)
let sr_wartosc x =
  let minimum = min_wartosc x and maksimum = max_wartosc x
  in
  if minimum = neg_infinity || maksimum = infinity
    then nan
  else (minimum +. maksimum) /. 2 ;;
