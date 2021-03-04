(* Reminder: If a test case requires multiple arguments, use a tuple:
let myfn_with_2_args_tests = [
  ((arg1, arg1), (expected_output))
]
*)

(* Q1 *)
(* TODO: Write a good set of tests for compress *)
let compress_tests = [
  ([A;A;A;G;G;A;T;T;T;C;T;C], [(3,A); (2, G); (1, A); (3, T); (1, C); (1, T); (1, C)]) ;
  ([G;G;T;C;C] , [(2,G); (1,T); (2,C)]);
  ([], []);
  ([A;A;A], [3,A]);
  ([A], [1,A]);
  ([T; A; C; G], [(1,T); (1,A); (1,C); (1,G)]);
  ([T;A;C;G], [(1, T); (1, A); (1, C); (1, G)])
]

(* TODO: Implement compress. *)
let compress (l : nucleobase list) : (int * nucleobase) list =
  (* raise NotImplemented *) 
  let rec compress_tr l acc = match l with
    (* reverse list because compressed list appends to head for PMing purposes*)
    | [] -> List.rev acc
    | nucIn::tailIn -> 
        match acc with 
        (* if empty, this is first nuclotide *)
        | [] -> compress_tr tailIn ((1, nucIn)::[])
        | (count, nucOut)::tailOut -> 
            if nucIn = nucOut then compress_tr tailIn ((count + 1, nucOut)::tailOut)
            else compress_tr tailIn ((1, nucIn)::(count, nucOut)::tailOut)
  in
  compress_tr l []

(* TODO: Write a good set of tests for decompress *)
let decompress_tests = [
  ([(1,T);(1,A);(1,C);(1,G)], [T;A;C;G]);
  ([(1,A)], [A]);
  ([], []) 
]

(* TODO: Implement decompress. *)
let rec decompress (l : (int * nucleobase) list) : nucleobase list =
  (* raise NotImplemented *)
  match l with
  | [] -> []
  | (count, nuc)::tail -> 
      (* if there are 2 or more nuc's, keep reducing the count*)
      if (count - 1 > 0) then nuc::(decompress ((count - 1, nuc)::tail))
(* else, this is the last nuc, add and go to next element in list *)
      else nuc::(decompress tail)

(* Q2 *)
(* TODO: Write a good set of tests for eval *) 
let eval_tests = [
  (FLOAT(1.), 1.);
  (PLUS(FLOAT(2.),PLUS(FLOAT(2.),FLOAT(2.))), 6.);
  (MINUS(FLOAT(4.),FLOAT(3.)), 1.);
  (MULT(FLOAT(2.),FLOAT(2.)), 4.);
  (DIV(FLOAT(4.),FLOAT(2.)), 2.);
  (SIN(FLOAT(3.14159265358979312)), 1.22464679914735321e-16);
  (COS(FLOAT(3.14159265358979312)), -1.);
  (EXP(FLOAT(0.)), 1.);
  (COS(SIN(MULT(MINUS(PLUS(FLOAT(3.), FLOAT(3.)), FLOAT(5.)), FLOAT(5.)))), 0.574400879193934);
  (DIV(SIN(FLOAT(3.14159265358979312)), COS(FLOAT(3.14159265358979312))), -1.22464679914735321e-16);
  (EXP(EXP(FLOAT(9.))), infinity);
]

(* TODO: Implement eval. *)
let rec eval e =
  (* raise NotImplemented *)
  match e with
  | FLOAT(n) -> n (* base case *)
  | PLUS(ex1, ex2) -> eval(ex1) +. eval(ex2)
  | MINUS(ex1, ex2) -> eval(ex1) -. eval(ex2)
  | MULT(ex1, ex2) -> eval(ex1) *. eval(ex2)
  | DIV(ex1, ex2) -> eval(ex1) /. eval(ex2)
  | SIN(ex1) -> sin(eval(ex1))
  | COS(ex1) -> cos(eval(ex1))
  | EXP(ex1) -> exp(eval(ex1))

(* TODO: Write a good set of tests for to_instr *)
let to_instr_tests = [
  (FLOAT(1.), [Float(1.)]);
  (EXP(FLOAT(0.)), [Float(0.); Exp]);
  (COS(FLOAT(3.14)), [Float(3.14); Cos]);
  (SIN(FLOAT(3.14)), [Float(3.14); Sin]);
  (PLUS(FLOAT(3.), FLOAT(3.)), [Float(3.); Float(3.); Plus]);
  (MINUS(FLOAT(3.), FLOAT(3.)), [Float(3.); Float(3.); Minus]);
  (MULT(FLOAT(3.), FLOAT(3.)), [Float(3.); Float(3.); Mult]);
  (DIV(FLOAT(3.), SIN(FLOAT(3.14))), [Float(3.); Float(3.14); Sin; Div]);
  (MULT (PLUS (FLOAT 2., FLOAT 3.), PLUS (FLOAT 2.2, FLOAT 3.3)), [Float(2.); Float(3.); Plus; Float(2.2); Float(3.3); Plus; Mult]);
]

(* TODO: Implement to_instr. *)
let rec to_instr e = 
  (* raise NotImplemented *)
  match e with
  | FLOAT(n) -> [Float(n)] (* base case *)
  | MULT(exL, exR) -> (to_instr exL)@(to_instr exR)@[Mult] (* Traverse each L and R branch, operators are internal nodes, leaves are operands *)
  | PLUS(exL, exR) -> (to_instr exL)@(to_instr exR)@[Plus]
  | MINUS(exL, exR) -> (to_instr exL)@(to_instr exR)@[Minus]
  | DIV(exL, exR) -> (to_instr exL)@(to_instr exR)@[Div]    
  | SIN(ex) -> (to_instr ex)@[Sin]
  | COS(ex) -> (to_instr ex)@[Cos]
  | EXP(ex) -> (to_instr ex)@[Exp]


(* TODO: Write a good set of tests for instr *)
let instr_tests = [
  ((Mult, [5.0; 5.5]), Some [27.5]);
  ((Exp, [3.;4.;2.]), Some [exp(3.);4.;2.]);
  ((Float(3.), [3.;2.]), Some [3.;3.;2.]);
  ((Plus, [1.]), None);
  ((Plus, []), None); 
]


(* TODO: Implement to_instr. *)               
let instr i s = 
  (* Note that we reverse the operands because 2nd is first in due to stacking *)
  (* raise NotImplemented *) 
  if s = [] then None
  else match i with
    | Float(n) -> Some(n::s) (* pushes number in stack *)
    | Exp ->
        let h::tail = s in Some(exp(h)::tail)
    | Sin -> 
        let h::tail = s in Some(sin(h)::tail)
    | Cos -> 
        let h::tail = s in Some(cos(h)::tail)
    | Plus -> (match s with
        | _::[] -> None
        | n1::n2::tail -> Some((n2 +. n1)::tail))
    | Minus -> (match s with
        | _::[] -> None
        | n1::n2::tail -> Some((n2 -. n1)::tail))
    | Mult -> (match s with
        | _::[] -> None
        | n1::n2::tail -> Some((n2 *. n1)::tail))
    | Div -> (match s with
        | _::[] -> None
        | n1::n2::tail -> Some((n2 /. n1)::tail)) 
(* TODO: Write a good set of tests for prog *)
let prog_tests = [
  ([Float 2.2; Float 3.3; Plus; Float 5.; Mult], Some(27.5));
  ([], None);
  ([Float(3.); Float(3.14); Sin; Div], Some(1883.64958174162916));
  ([Float (2.); Exp; Exp], Some(1618.17799191265385));
  ([Float(2.); Plus], None);
  ([Float 1.36513149163213021; Float 1.82107608901025131; Mult; Exp; Exp;
    Float 0.878990007711675458; Plus; Float 0.790119693878581497; Minus; Exp;
    Float 1.43665133606194262; Plus; Sin; Sin], Some(nan));
]

(* TODO: Implement prog. *)
let prog instrs = 
  (* raise NotImplemented 
we need to determine if next element is an operator, to find instr function*)
  let rec prog_help (inst:instruction list) (s:stack) : float option =
    match inst with
    | [] -> (* when empty, means stack has final value or an ill-formed list of instr*) 
        (match s with
         | (_::_::_ | []) -> (None) (* ill-formed cases where we have at least two values or an empty stack *)
         | (value::[]) -> Some(value)
        )
    | h::t -> match h with
      | Float(n) -> (prog_help (t) (n::s))
      | operator -> match (instr operator s) with
        (* returns either None, or a new stack *)
        | None -> None 
        | Some(newStack) -> (prog_help (t) (newStack))
  in 
  prog_help instrs [] 