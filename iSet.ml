type t =
  | Empty
  | Node of t * (int * int) * t * int * int
;;

let add_num a b =
  let (a, b) = ((min a b), (max a b)) in
  if a >= 0 then
    if a < max_int - b then a + b else max_int
  else
    if b > min_int - a then a + b else min_int
;;

let height = function
  | Node(_, _, _, h, _) -> h
  | Empty -> 0
;;

let element = function
  | Node (_, _, _, _, e) -> e
  | Empty -> 0
;;

let make l ((a, b) as k) r =
  let (a, b) =  (if a = min_int then (a+1, (add_num b 1)) else a, b ) in
    let dlugosc = add_num (add_num b (-a)) 1
    and reszta = add_num (element l) (element r)
  (*let ile = add_num (element l) (add_num (element r) (add_num (-a) (add_num b 1)) )*)
    in let ile = add_num dlugosc reszta
    and h = (max(height l) (height r)) +1
  in Node(l, k, r, h, ile)
;;
(*
let bal l k r =
  let hl = height l
  and hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lr, _, _) ->
      if height ll >= height lr
        then make ll lk (make lr k r)
      else
        (match lr with
        | Node (lrl, lrk, lrr, _, _) ->
          make (make ll lk lrl) lrk (make lrr k r)
        | Empty -> assert false)
    | Empty -> assert false
  else
    if hr > hl + 2 then
      match r with
      | Node (rl, rk, rr, _, _) ->
        if height rr >= height rl
          then make (make l k rl) rk rr
        else
          (match rl with
          | Node (rll, rlk, rlr, _, _) ->
            make (make l k rll) rlk (make rlr rk rr)
          | Empty -> assert false)
      | Empty -> assert false
    else make l k r
;;*)


let bal l k r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lr, _, _) ->
        if height ll >= height lr then make ll lk (make lr k r)
        else
          (match lr with
          | Node (lrl, lrk, lrr, _, _) ->
              make (make ll lk lrl) lrk (make lrr k r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _, _) ->
        if height rr >= height rl then make (make l k rl) rk rr
        else
          (match rl with
          | Node (rll, rlk, rlr, _, _) ->
              make (make l k rll) rlk (make rlr rk rr)
          | Empty -> assert false)
    | Empty -> assert false
  else make l k r
;;

let rec min_elt = function
  | Node (Empty, k, _, _, _) -> k
  | Node (l, _, _, _, _) -> min_elt l
  | Empty -> raise Not_found
;;

let rec remove_min_elt = function
  | Node (Empty, _, r, _, _) -> r
  | Node (l, k, r, _, _) -> bal (remove_min_elt l) k r
  | Empty -> invalid_arg "iSet.remove_min_elt"
;;

let rec max_elt = function
  | Node (_, k, Empty, _, _) -> k
  | Node (_, _, r, _, _) -> max_elt r
  | Empty -> raise Not_found
;;

let rec remove_max_elt = function
  | Node (l, _, Empty, _, _) -> l
  | Node (l, k, r, _, _) -> bal l k (remove_max_elt r)
  | Empty -> invalid_arg "iSet.remove_max_elt"
;;


let merge t1 t2 =
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _ ->
    let k = min_elt t2 in
    bal t1 k (remove_min_elt t2)
;;

let empty = Empty;;

let is_empty set =
  set = Empty;;

let rec add_one ((a, b) as x) = function
  | Node (l, ((c, d) as k), r, _, _) ->
    if b < c then
      let left = add_one x l in bal left k r
    else
      let right = add_one x r in bal l k right
  | Empty -> make Empty x Empty
;;

let rec join l v r =
  match (l, r) with
  | (Empty, _) -> add_one v r
  | (_, Empty) -> add_one v l
  | (Node (ll, lv, lr, lh, _), Node(rl, rv, rr, rh, _)) ->
    if rh + 2 < lh then bal ll lv (join lr v r)
    else
      if rh > lh + 2 then bal (join l v rl) rv rr
      else make l v r
;;

let split x set =
  let rec loop x = function
  | Empty -> (Empty, false, Empty)
  | Node (l, ((a, b) as v), r, _, _) ->
      if (a <= x && x <= b) then
        let l_new = if a = x then l else add_one (a, x-1) l
        and r_new = if b = x then r else add_one (x+1, b) r
        in (l_new, true, r_new)
      else
        if x < a then
          let (ll, pres, rl) = loop x l in (ll, pres, join rl v r)
        else
          let (lr, pres, rr) = loop x r in (join l v lr, pres, rr)
  in loop x set ;;

let rec add (a, b) set =
  let (l, _, _) = split a set
  and (_, _, r) = split b set
  in
  let (pocz, pocz2) = if is_empty l then ( (add_num a (-2)), (add_num a (-2)) ) else max_elt l
  and (kon, kon2) = if is_empty r then ( (add_num b (-2)), (add_num b (-2)) ) else min_elt r
  in
  if add_num pocz2 1 = a then
    if add_num b 1 = kon
      then join (remove_max_elt l) (pocz, kon2) (remove_min_elt r)
      else join (remove_max_elt l) (pocz, b) r
  else
    if add_num b 1 = kon
      then join l (a, kon2) (remove_min_elt r)
      else join l (a, b) r
;;

let remove ((a, b)) set =
  let (l, _, _) = split a set
  and (_, _, r) = split b set
  in
  match (l, r) with
  | (Empty, _) -> r
  | (_, Empty) -> l
  | (_,_) -> join l (min_elt r) (remove_min_elt r);;



let mem a set =
  let rec loop = function
  | Empty -> false
  | Node (l, (c, d), r, _, _) ->
    (a >= c && a <= d) || loop ( if a < c then l else r )
  in loop set;;


let iter f set =
  let rec loop = function
  | Empty -> ()
  | Node (l, k, r, _, _) -> loop l; f k; loop r in
  loop set;;

let fold f set acc =
  let rec loop acc = function
  | Empty -> acc
  | Node (l, k, r, _, _) -> loop (f k (loop acc l)) r in
  loop acc set;;


let elements set =
  let rec loop acc = function
  | Empty -> acc
  | Node (l, k, r, _, _) -> loop (k :: (loop acc r)) l in
  loop [] set;;


let rec below a set =
  let (l, x, _) = split a set in
  add_num (element l) (if x then 1 else 0);;
