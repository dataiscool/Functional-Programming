(* TODO: Write some tests for neighbours. Consider creating a graph first,
 and then writing your tests based on it *)

(* Reminder: If a test case requires multiple arguments, use a tuple:
let myfn_with_2_args_tests = [
  ((arg1, arg1), (expected_output))
]
*)

(* We've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
let neighbours_tests: ((string graph * string) * (string * weight) list) list = 
  let w: weight = 5 in
  let a = {nodes = ["a";"b"]; edges = [ ("a","b", w) ] } in
  [ 
    ((a, "a"), [("b",w)]); ((a, "b"), []); 
  ]

(* TODO: Implement neighbours. *)
let neighbours (g: 'a graph) (vertex: 'a) : ('a * weight) list =
  (*raise NotImplemented*)
  let rec traverse nexEdge nList = match nexEdge with
    | [] -> nList
    | (v1, v2, w)::l -> if vertex = v1 then traverse l (nList@[(v2,w)])
        else traverse l nList
  in
  traverse g.edges []

(* TODO: Implement find_path. *)
let find_path (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) =
  let rec aux_node (node: 'a * weight) (visited : 'a list) : ('a list * weight) = 
    let (v,w) = node in 
    if  List.mem v visited then raise Fail
    else if v = b then ([v],w)
    else let (aNext,wNext) = aux_list (neighbours g v) (v::visited) in (v::aNext,w+wNext)
    
  and aux_list (nodes: ('a * weight) list) (visited: 'a list) : ('a list * weight) =
    match nodes with
    | [] -> raise Fail
    | (v,w)::rest -> try aux_node (v,w) visited 
        with Fail -> aux_list rest (v::visited) (* This also catches Fail from both; if it reaches
                                           a dead end, it will backtrack to start from another
                                           viable route in rest of neighbours *)
  in
  aux_node (a,0) [] (* pass starting node a, weight 0 because it's itself *)
    
(* TODO: Implement find_path'. *)
let find_path' (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) =
  let rec aux_node (node: 'a * weight) (visited : 'a list) fc sc : ('a list * weight)=
    let (v,w) = node in
    if List.mem v visited then fc ()
    else if v = b then sc ([v],w)
    else aux_list (neighbours g v) (v::visited) fc (fun (aN,wN) -> sc (v::aN, w+wN))
  and aux_list (nodes: ('a * weight) list) (visited: 'a list) fc sc : ('a list * weight) =
    match nodes with
    | [] -> fc ()
    | (v,w)::rest -> aux_node (v,w) visited (fun () -> aux_list rest (v::visited) fc sc) sc  
  in
  aux_node (a,0) [] (fun () -> raise Fail) (fun x -> x)

  
(* TODO: Implement find_all_paths *) (* Can't use exceptions*)
let find_all_paths (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) list = 
  let rec aux_node (node: 'a * weight) (visited : 'a list) : ('a list * weight) list = 
    let (v,w) = node in 
    if  List.mem v visited then []
    else if v = b then [([v],w)]
    else (List.map (fun (a,w1) -> (v::a,w+w1)) (aux_list (neighbours g v) (v::visited)))
  and aux_list (nodes : ('a * weight) list) (visited : 'a list) : ('a list * weight) list  = 
    match nodes with 
    | [] -> []
    | (v,w)::rest -> (aux_node (v,w) visited)@(aux_list rest visited)
  in 
  aux_node (a,0) []
    
(* TODO: Implement find_shortest_path *)
let find_shortest_path (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) option = 
  match List.sort (fun (_,wa) (_,wb) -> if wa < wb then -1 
                    else if wa > wb then +1 
                    else 0) (find_all_paths g a b) with
  | [] -> None
  | (v,w)::_ -> Some(v,w) 

