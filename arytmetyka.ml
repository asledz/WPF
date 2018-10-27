(* Zadanie Arytmetyka przybliżonych wartości *)
(* Anita Śledź 406384 *)
(* Code-review : Gerard Dróżdż *)

(*
Spójnym nazywam przedział w formie <a,b>
Niespójnym nazywam przedział w formie <-inf,a>u<b,inf>
*)

type wartosc =
  Spojny of (float * float)
  | Niespojny of (float * float) ;;

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

(*============ FUNKCJE POMOCNICZE ============*)


(*Funkcja sprawdzająca, czy przedział jes ok, przydaje się w dodawaniu i odejmowaniu*)
let redukuj x =
  match x with
  | Spojny(a, b) ->
    if(a >= b) then Spojny(neg_infinity, infinity)
    else x
  | Niespojny(a, b) ->
    if( a >=  b) then Spojny(neg_infinity, infinity)
    else x;;

(* Funkcja zmieniająca przedział na przedział jemu przeciwny(ujemny), przydaje się w odejmowaniu*)
let przeciwny x =
  match x with
  | Spojny (a, b) -> Spojny ( -.b , -.a )
  | Niespojny ( a, b ) -> Niespojny ( -.b -.a) ;;

(* Funkcja porządkująca przedziały - jeśli a > b, to swap a z b , przydaje się w odwrotności*)
let porzadkuj x =
  match x with
  | Spojny (a, b) ->
    if ( a > b ) then Spojny (b, a)
    else Spojny (a, b)
  | Niespojny(a, b) ->
    if( a > b ) then Niespojny (b, a)
    else Niespojny (a, b);;

(*Funkcja licząca odwrotność przedziału *)
let odwrotny x =
  match x with
  | Spojny( a, 0. ) -> Spojny (neg_infinity , 1 /. a)
  | Spojny( 0. , b) -> Spojny (1 /. b , infinity)
  | Spojny( a, b ) ->
    if ( a < 0 && b > 0) then porzadkuj (Niespojny(1 /. a , 1 /. b)) (*Zawiera w sobie zero - obraca się w drugą stronę*)
    else porzadkuj ( Spojny(1 /. a , 1 /. b) )
  | Niespojny (a, b) ->
    if( a > 0 || b < 0) then porzadkuj ( Niespojny (1 /. a, 1 /. b) ) (*Zawiera w sobie zero*)
    else porzadkuj ( Spojny ( 1 /. a, 1 /.b ) )  ;;


(*============ MODYFIKATORY ============*)

let rec plus x y =
  match x, y with
  | Spojny( a , b ) , Spojny( c , d ) -> redukuj ( Spojny ( a +. c , b +. d) )
  | Spojny( a , b ) , Niespojny( c , d ) -> redukuj ( Niespojny ( c +. a , d +.b ) )
  | Niespojny( a , b ) , Spojny ( c , d ) -> plus y x
  | Niespojny( a , b ) , Niespojny ( c , d ) -> redukuj ( Spojny (neg_infinity, infinity) );; (*Suma może być jakakolwiek*)

let minus x y =
  plus x (przeciwny y);;


let rec razy x y =

;;

let rec dziel x y =
  razy x ( odwrotny y) ;;
