(* https://gist.github.com/23Skidoo/1347308 *)

type 'a queue = Queue of 'a list * 'a list

let empty = Queue ([], []);;

let add q elt = match q with
  | Queue (front, back) -> Queue (elt::front, back);;

let take q = match q with
  | Queue ([], []) -> raise (Invalid_argument "Empty queue!")
  | Queue (front, b::bs) -> b, (Queue (front, bs))
  | Queue (front, []) -> let back = List.rev front
in (List.hd back, Queue ([], List.tl back));;
