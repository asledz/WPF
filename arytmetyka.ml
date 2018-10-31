(* Zadanie Arytmetyka przybliżonych wartości *)
(* Anita Śledź 406384 *)
(* Code-review : Gerard Dróżdż *)

(*
Spójnym nazywam przedział w formie <a,b>
Niespójnym nazywam przedział w formie <-inf,a>u<b,inf>
*)

type wartosc =
  Spojny of (float * float)
  | Niespojny of (float * float);;

(*============ KONSTRUKTORY ============*)

let wartosc_dokladnosc x p =
  let blad = abs_float (x *. (p /. 100.0))
  in Spojny (x -. blad, x +. blad);;

let wartosc_od_do x y =
  Spojny (x, y);;

let wartosc_dokladna x =
  Spojny (x, x);;

(*============ SELEKTORY ============*)

(* Funkcja sprawdzająca, czy w przedziale x znajduje się float y *)
let in_wartosc x y =
  match x with
  | Niespojny (lewy, prawy) -> y <= lewy || prawy <= y
  | Spojny (lewy, prawy) -> lewy <= y && y <= prawy;;

(* Funkcja znajdująca najmniejszą wartość przedziału x *)
let min_wartosc x =
  match x with
  | Niespojny (a, b) ->
    if a = neg_infinity && b = infinity then nan
    else neg_infinity
  | Spojny (a, _) -> a;;

(* Funkcja znajdująca największą wartość przedziału x *)
let max_wartosc x =
  match x with
  | Niespojny (a, b) ->
    if a = neg_infinity && b = infinity then nan
    else infinity
  | Spojny (_, b) -> b;;

(* Funkcja znajdująca średni element przedziału x *)
let sr_wartosc x =
  if x = Niespojny (neg_infinity, infinity) then nan
  else
    let minimum = min_wartosc x and maksimum = max_wartosc x
    in (minimum +. maksimum) /. 2.;;

(*============ FUNKCJE POMOCNICZE ============*)

(* Funkcja zwracająca wartość bezwzględną *)
let modul x:float =
  if x < 0. then  x *. -1.
  else x;;

(* Funkcja zmieniająca przedział na przedział jemu przeciwny(ujemny), przydaje się w odejmowaniu *)
let przeciwny x =
  match x with
  | Spojny (a, b) -> Spojny (-1. *. b, -1. *. a)
  | Niespojny (a, b) ->
    if a = neg_infinity && b = infinity then x
    else Niespojny (-1. *. b, -1. *. a);;

(* Funkcja porządkująca przedziały - jeśli a > b, to swap a z b , przydaje się w odwrotności *)
let porzadkuj x =
  match x with
  | Spojny (a, b) ->
    if (a > b) then Spojny (b, a)
    else Spojny (a, b)
  | Niespojny (a, b) ->
    if (a > b) then Niespojny (b, a)
    else Niespojny (a, b);;

(* Funkcja licząca odwrotność przedziału *)
let odwrotny x =
  match x with
  | Niespojny (a, b) ->
    if (a > 0. && b < 0.) then porzadkuj (Niespojny (1. /. a, 1. /. b))
    else porzadkuj (Spojny ( 1. /. a, 1. /.b ))
  | Spojny (a, b) ->
    if modul a = 0. && modul b = 0. then Niespojny (neg_infinity, infinity)
    else
      if modul a = 0. then Spojny (1. /. b, infinity)
      else
        if modul b = 0. then Spojny (neg_infinity, 1. /. a)
        else
          if a < 0. && b > 0. then porzadkuj (Niespojny (1. /. a, 1. /. b))
          else porzadkuj (Spojny (1. /. a, 1. /. b));;

(* Funkcja eliminująca nany *)
let mnoz_ignoruj_nan a b =
  if a *. b <> a *. b then 0.
  else a *. b;;

(* Funkcja zwracająca wynik pomnożenia dwóch przedziałów spójnych *)
let mnozenie_spojnych x y =
  match x, y with
  | Spojny (a, b), Spojny (c, d) ->
    if (modul a = 0. && modul b = 0.) || (modul c = 0. && modul d = 0.) then Spojny (0., 0.)
    else
      let lewy = min (min (mnoz_ignoruj_nan a c) (mnoz_ignoruj_nan a d)) (min (mnoz_ignoruj_nan b c) (mnoz_ignoruj_nan b d))
      and prawy = max (max (mnoz_ignoruj_nan a c) (mnoz_ignoruj_nan a d)) (max (mnoz_ignoruj_nan b c) (mnoz_ignoruj_nan b d))
      in Spojny (lewy, prawy)
  | _, _ -> Spojny (0., 0.);;


(* Funkcja zwracająca połączone dwa przedziały *)
let rec lacz x y =
    match x, y with
    | Spojny (a, b), Spojny (c, d) ->
      if a < c then
        if b >= c then Spojny (a, max b d)
        else Niespojny (b, c)
      else
        if d >= a then Spojny (c, max b d)
        else Niespojny (d, a)
    | Niespojny (a, b), Spojny (c, d) ->
      let przed1 = Spojny (neg_infinity, (if a >= c then d else a))
      and przed2 = Spojny ((if b <= d then c else b), infinity)
      in lacz przed1 przed2
    | Spojny (c, d), Niespojny (a, b) ->
      let przed1 = Spojny (neg_infinity, (if a >= c then d else a))
      and przed2 = Spojny ((if b <= d then c else b), infinity)
      in lacz przed1 przed2
    | Niespojny (a, b), Niespojny (c, d) ->
      let przed1 = Spojny (neg_infinity, max a c)
      and przed2 = Spojny (min b d, infinity)
      in lacz przed1 przed2;;

(*============ MODYFIKATORY ============*)

let rec plus x y =
  if x = Niespojny (neg_infinity, infinity) || y = Niespojny (neg_infinity, infinity)
    then Niespojny (neg_infinity, infinity)
  else
    match x, y with
    | Spojny (a, b), Spojny (c, d) ->
      Spojny (a +. c, b +. d)
    | Niespojny (a, b), Spojny (c, d) ->
      lacz (Spojny (neg_infinity, a +. d)) (Spojny (b +. c, infinity))
    | Spojny (a, b), Niespojny (c, d) ->
      lacz (Spojny (neg_infinity, b +. c)) (Spojny (a +. d, infinity))
    | Niespojny (a, b), Niespojny (c, d) ->
      Spojny (neg_infinity, infinity);;

let minus x y =
  plus x (przeciwny y);;

let rec razy x y =
  if x = Niespojny (neg_infinity, infinity) || y = Niespojny (neg_infinity, infinity)
    then Niespojny (neg_infinity, infinity)
  else
    match x, y with
    | Spojny (a, b), Spojny (c, d) -> mnozenie_spojnych x y
    | Niespojny (a, b), _ ->
      let przed1 = Spojny (neg_infinity, a)
      and przed2 = Spojny (b, infinity)
      in lacz (razy y przed1) (razy y przed2)
    | _ , _ -> razy y x
;;

let rec podzielic x y =
  (* Jakikolwiek pusty przedział - pusty przedział *)
  if x = Niespojny (neg_infinity, infinity) || y = Niespojny (neg_infinity, infinity)
    then Niespojny (neg_infinity, infinity)
  else
    match y with
    | Spojny (a , b) ->
      (* Dzielenie przez 0 *)
      if modul a = 0. && modul b = 0. then Niespojny (neg_infinity, infinity)
      else
        if a = neg_infinity && b = infinity then razy x y
        else razy x (odwrotny y)
    | Niespojny (a, b) ->
      let przedz1 = Spojny (neg_infinity, a)
      and przed2 = Spojny (b, infinity)
      in lacz (podzielic x przedz1) (podzielic x przed2);;
