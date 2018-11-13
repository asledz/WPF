(*
Drzewa Lewicowe
Anita Śledź 406384
Review: Lara Citko
*)


(* Typ trzymający drzewo:
  1) Liść - jest pusty;
  2) Wierzchołek - przechowuje lewe_poddrzewo * prawe_poddrzewo * wartość * głębokość
*)

type 'a queue =
  | Leaf
  | Node of 'a queue * 'a queue * 'a * int
;;


(* Tworzy pustą kolejkę - liść *)
let empty = Leaf;;

let is_empty q =
  match q with
  | Leaf -> true
  | Node(_, _, _, _) -> false
;;

let depth q =
  match q with
  | Leaf -> 0
  | Node(_, _, _, res) -> res
;;


let rec join q p =
  match q, p with
  | Leaf, pom2 -> pom2
  | pom1, Leaf -> pom1
  | Node(ql, qp, qx, qd), Node(pl, pp, px, pd) ->
    if(qx > px) then
      join p q
    else
      let pom = join qp p in
      if(depth ql < depth pom) then
        Node(pom, ql, qx, (depth ql) + 1)
      else
        Node(ql, pom, qx, (depth pom) + 1)
;;

let add e q =
  join (Node (Leaf, Leaf, e, 1)) q
;;

exception Empty;;

let delete_min q =
  match q with
  | Leaf -> raise Empty
  | Node(q1, q2, x, depth) -> (x, join q1 q2)
;;
