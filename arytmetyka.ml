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
  else (minimum +. maksimum) /. 2. ;;

(*============ FUNKCJE POMOCNICZE ============*)


(*Funkcja sprawdzająca, czy przedział jest ok, przydaje się w dodawaniu i odejmowaniu, do redukowania przedziałów niespójnych od neg inf do inf*)
let napraw x =
  match x with
  | Niespojny(a, b) ->
    if( a >=  b) then Spojny(neg_infinity, infinity)
    else x
  | Spojny(_,_) -> x;;

(* Funkcja zmieniająca przedział na przedział jemu przeciwny(ujemny), przydaje się w odejmowaniu*)
let przeciwny x =
  match x with
  | Spojny (a, b) -> Spojny ( -1. *. b , -1. *. a )
  | Niespojny ( a, b ) -> Niespojny ( -1. *. b , -1. *. a) ;;

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
  | Niespojny (a, b) ->
  if( a > 0. || b < 0.) then porzadkuj ( Niespojny (1. /. a, 1. /. b) )
  else porzadkuj ( Spojny ( 1. /. a, 1. /.b ) )
  | Spojny( a, 0. ) -> Spojny (neg_infinity , 1. /. a)
  | Spojny( 0. , b) -> Spojny (1. /. b , infinity)
  | Spojny( a, b ) ->
    if ( a < 0. && b > 0.) then porzadkuj (Niespojny(1. /. a , 1. /. b))
    else porzadkuj ( Spojny(1. /. a , 1. /. b) )
;;

(* w tej funkcji trzeba jeszcze wyifować nany jeśli jest to potrzebne??*)
let mnozenie_spojnych x y =
  match x, y with
  | Spojny(a, b), Spojny(c, d) ->
    Spojny ( ( min (min (a*.c) (a*.d ) ) (min (b*.c)(b*.d) ) )
          ,( max ( max(a *. c) (a *. d) ) ( max( b *.c ) (b *. d) ) ) )
  | _,_ -> Spojny(0. , 0.);;

let rec lacz x y =
    match x, y with
    | Spojny (a , b), Spojny(c, d) ->
      if a < c then
        if b >= c then Spojny(a, d)
        else Niespojny( b, c )
      (*else lacz y x (*inaczej zamień je miejscami*) *)
      else
        if d >= a then Spojny(c, b)
        else Niespojny(d, a)
    | Niespojny(a, b) , Spojny(c, d) ->
      let przed1 = Spojny (neg_infinity, (if a >= c then d else a) )
      and przed2 = Spojny ( (if b <= d then c else b), infinity )
      in lacz przed1 przed2
    | Spojny( _,_ ) , Niespojny( _,_ ) -> lacz y x (*Zmieniam kolejność*)
    | Niespojny(a, b), Niespojny(c, d) ->
      let przed1 = Spojny(neg_infinity, max a c)
      and przed2 = Spojny(min b d, infinity)
      in lacz przed1 przed2
;;

let modul x =
  if x < 0 then -1 *. x
  else x ;;

(*============ MODYFIKATORY ============*)

let rec plus x y =
  match x, y with
  | Spojny( a , b ) , Spojny( c , d ) -> napraw ( Spojny ( a +. c , b +. d) )
  | Spojny( a , b ) , Niespojny( c , d ) -> napraw ( Niespojny ( c +. a , d +.b ) )
  | Niespojny( a , b ) , Spojny ( c , d ) -> plus y x
  | Niespojny( a , b ) , Niespojny ( c , d ) -> napraw ( Spojny (neg_infinity, infinity) );; (*lacz może być jakakolwiek*)

let minus x y =
  plus x (przeciwny y);;


let rec razy x y =
  match x, y with
  | Spojny (a, b) , Spojny (c, d) -> mnozenie_spojnych x y
  | Niespojny(a, b) , _ ->
    let przed1 = Spojny (neg_infinity, a)
    and przed2 = Spojny (b, infinity)
    in lacz ( razy y przed1 ) (razy y przed2)
  | _ , _ -> razy y x
;;

let modul a:float =
  if a < 0. then a *. -1.
  else a;;

let rec podzielic x y =
  match y with
  | Spojny (a , b) ->
    if modul(a) = 0. && modul(b) = 0. then Niespojny(neg_infinity , infinity)
    else
      if (a = neg_infinity && b = infinity) then razy x (Spojny (a, b))
      else razy x (odwrotny y)
  | Niespojny (a, b) -> lacz( podzielic x (Spojny(neg_infinity, a)) ) ( podzielic x(Spojny(b, infinity)) );;

