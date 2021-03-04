(* TODO: Write some tests for tabulate. *)
(* Note: we've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
let tabulate_tests: (((int -> int) * int) * int list) list = [
  (* Remember: your test cases should have this form:
     ((f, n), output)
     The following test case asserts that:
       tabulate (fun x -> x) (-1)
     should have the output []
  *)
  (((fun x -> x), -1), []); 
  (((fun x -> 2 * x), 3), [0; 2;4;6]);
  (((fun x -> x / 2), 3), [0; 0; 1; 1]);
  (((fun x -> x), 0), [0]);
]

(* TODO: Implement dist_table: (int * int) -> int -> float list *)
let dist_table ((marblesTotal, marblesDrawn): (int * int)) (x: int) : float list =
  (* n {number of black marbles in jar} needs to be last value dist_table waits on
  since tabulate takes arguments f n *)
  let swap f = fun (marblesTotal, marblesDrawn) x n -> f n x (marblesTotal, marblesDrawn) 
  in
  let dist_black' = swap dist_black (marblesTotal, marblesDrawn) x
  in
  tabulate dist_black' marblesTotal (* P_N(X=x), where N from 0 to marblesTotal *)

(* TODO: Write some test cases for is_empty. *)
let is_empty_tests: (float list list * bool) list = [
  ([ [] ], true);
  ([ [0.;1.;2.] ], false);
  ([ [ 0.] ], false);
  ([ [0.]; [0.] ], false);
  ([ []; []], true);
]

(* TODO: Implement is_empty: 'a list list -> bool *)
let is_empty (matrix: 'a list list) : bool = 
  List.for_all (fun l -> l = []) matrix

(* TODO: Implement dist_matrix: int * int -> int list -> float list list *)
let dist_matrix ((total, drawn): int * int) (resultList: int list) : float list list = 
  (* create new function from dist_table by partial eval of type int -> float list for List.map *) 
  List.map (dist_table (total, drawn)) resultList
(* equivalent to [dist_table' resultList-element1; dist_table' resultList-element2; etc.] *)
    
(* TODO: Implement combined_dist_table: float list list -> float list *)
let rec combined_dist_table (matrix: float list list) =
  (* use head as the base case for fold_right *) 
  let head = List.hd matrix in 
  let inter = List.fold_right (List.map2 (fun x y -> x*.y)) matrix head in
  (* Remove duplicate head from list *)
  List.map2 (fun x y -> if y <> 0. then x /. y else 0.) inter head

(* Once you have implemented all the above functions, you can
   use this function to compute the maximum likelihood.
   You can try it on the given example by running:
     max_likelihood (6, 3) [2; 0; 1]
*)
let max_likelihood (total, drawn) resultList =
  max_in_list
    (combined_dist_table
       (dist_matrix (total, drawn) resultList))


(* TODO: Implement all: (ingredients list -> bool) -> cake -> bool *)
let rec all (p: (ingredients list -> bool)) (c: cake) : bool = 
  (* raise NotImplemented *)
  match c with
  | Slice(ingL) -> p ingL
  | Cake(Slice(ingL), restCake) -> (p ingL) && all p restCake
  | Cake(c1,c2) -> all p c1 && all p c2
    
                     (* let rec ingList cake = match cake with
                         | Slice(ing) -> ing
                         | Cake(Slice(ing), restCake) -> ing@ingList(restCake)
                         | Cake(c1,c2) -> ingList(c1)@ingList(c2)
                      in
                      p (ingList c) *)
    
(* TODO: Write some test cases for is_chocolate_cake. *)
let is_chocolate_cake_tests = [
  (Cake (Slice [Chocolate ; Flour], Slice[]), false); 
  (Cake (Slice [Almonds ; Flour], Slice[]), false);
  (Cake (Slice [], Slice []), false); 
  (Cake (Slice [Chocolate], Cake ( Slice [Chocolate], Slice [Chocolate])), true);
]

(* TODO: Implement is_chocolate_cake: cake -> bool *)
let is_chocolate_cake (c: cake) : bool = 
  (*  *)  
  let rec findChoco l = match l with
    | [] -> false
    | Chocolate::_ -> true
    | _::ingL -> findChoco(ingL)
  in
  all findChoco c
  
(* TODO: Implement map: (ingredients list -> ingredients list) -> cake -> cake *)
let rec map (p: (ingredients list -> ingredients list)) (c: cake) = 
  (* Create a new cake recursively using function p *)
  match c with
  | Slice(ingL) -> Slice(p ingL)
  | Cake(Slice(ingL), restCake) -> Cake(Slice(p ingL), map p restCake)
  | Cake(c1, c2) -> Cake(map p c1, map p c2)

(* TODO: Write some test cases for add_ingredient. *)
let add_ingredient_tests = [
  ((Chocolate,Cake (Slice [Chocolate ; Flour], Slice[])), Cake (Slice [Chocolate ; Flour], Slice[Chocolate]));
  ((Chocolate, Cake (Slice [Almonds ; Flour], Slice[])), Cake (Slice [Almonds ; Flour; Chocolate], Slice[Chocolate]));
]

(* TODO: Implement add_ingredient: ingredients -> cake -> cake *)
let add_ingredient (x: ingredients) (c: cake) : cake = 
  (* Create new function from insert by partial eval to pass to map *)
  map (insert x) c

(* TODO: Implement fold_cake: (ingredients list -> 'a -> 'a) -> 'a -> cake -> 'a  *)
let rec fold_cake (f: (ingredients list -> 'a -> 'a)) (base: 'a) (c: cake) : 'a = 
  (* Note from function signature, implement fold_left *)  
  (* Note: 'base' is updated at every recursion; issa accumulator *)
  match c with 
  | Slice(ingL) -> (f ingL base)
  | Cake(Slice(ingL), restCake) -> fold_cake f (f ingL base) restCake
  | Cake(c1, c2) -> fold_cake f (fold_cake f base c1) c2
  
(* TODO: Implement get_all_ingredients: cake -> ingredients list *)
let get_all_ingredients (c: cake) : ingredients list = 
  (* Given union function adds unique l2 elements into l1
  We want to add cake elements (l1) into base accumulating unique values (l2)
  thus perform swap *)
  (* Assume each ingredient appears only once in each slice*)
  let swap f = fun l2 l1 -> f l1 l2 in
  let union' = swap union in
  fold_cake union' [] c
    
  