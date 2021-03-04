(* Question 1 *)
(* TODO: Write your own tests for the fact function.
         See the provided tests for double, above, for how to write test cases.
         Remember that you should NOT test cases for n < 0.
*)
(* TODO: Correct these tests for the fact function. *)
let fact_tests = [
  (0, 1.);
  (1, 1.);
  (2, 2.);
  (5, 120.)
]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let rec fact (n: int): float = match n with
  | 0 -> 1.0
  | _ -> float_of_int(n) *. fact (n - 1)


(* TODO: Write your own tests for the binomial function.
         See the provided tests for fact, above, for how to write test cases.
         Remember that we assume that  n >= k >= 0; you should not write test cases where this assumption is violated.
*)
let binomial_tests = [
  (* Your test cases go here. Correct the incorrect test cases for the function. *)
  ((0, 0), 1.);
  ((1, 0), 1.);
  ((1, 1), 1.);
  ((2, 0), 1.);
  ((10,1), 10.);
  ((10,2), 45.)
]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let binomial (n: int) (k:int): float =
  if n < 0 then domain ()
  else if k = n then 1.
  else fact n /. (fact k  *. fact (n-k))


(* TODO: Write a good set of tests for ackerman. *)
let ackerman_tests = [
  (* Your test cases go here *)
  ((0, 0), 1);
  ((0, 1), 2);
  ((0, 2), 3);
  ((0, 3), 4);
  ((0, 4), 5);
  ((1, 0), 2);
  ((1, 1), 3);
  ((3, 3), 61);
]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let ackerman (n, k): int  =
  if n < 0 || k < 0 then domain ()
  else (let rec ack n k = match (n,k) with
      | (0 , _ ) -> k + 1 
      | (_ , 0 ) -> ack (n-1) 1
      | (_ , _ ) -> ack (n-1) (ack n (k-1))
     in ack n k)


(* Question 2: is_prime *)

(* TODO: Write a good set of tests for is_prime. *)
let is_prime_tests = [
(* Your tests go here *) 
  (2, true);
  (3, true);
  (4, false);
  (5, true);
  (6, false);
  (10, false);
  (11, true);
  
]

(* TODO: Correct this implementation so that it compiles and returns
the correct answers.
*)
let is_prime n : bool =
  (* raise NotImplemented *)
  let rec prime mem =
    if mem = n || mem * mem > n then
      true
        (* else if mem * mem > n then
true *) 
    else if n mod mem = 0 then
      false
    else prime (mem + 1)
  in
  if n <= 1 then domain()
  else prime 2


(* Question 3: Newton-Raphson method for computing the square root
*)

let square_root_tests = [
  (1., 1.);
  (4., 2.);
  (9., 3.);
  (16., 4.);
  (36., 6.);
  (35., 5.9160797831);
]

let square_root a =
  let rec findroot x acc =
    let x' = ((a /. x) +. x) /. 2.0
    in
    if abs_float (x' -. x) <= acc then
      x'
    else findroot x' acc
  in
  if a > 0.0 then
    findroot 1.0 epsilon_float
  else domain ()

 
(* Question 4: Fibonacci*)

(* TODO: Write a good set of tests for fib_tl. *)
let fib_tl_tests = [
  (0, 1);
  (1, 1);
  (2, 2);
  (3, 3);
  (4, 5);
  (5, 8);
  (6, 13);
  (13, 377);
]

(* TODO: Implement a tail-recursive helper fib_aux. *)
let rec fib_aux n a b =
  (* raise NotImplemented *)
  let nextFib = a + b 
  in
  if n = 0 then
    a
  else fib_aux (n - 1) b nextFib

(* TODO: Implement fib_tl using fib_aux. *)
let fib_tl n =
  (* raise NotImplemented *)
  if n < 0 then domain ()
  else fib_aux n 1 1