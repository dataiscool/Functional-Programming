(*------------------ Q1------------------ *)
let rec parseExp (toklist: token list) (sc: (token list -> exp -> 'a)) : 'a =
  parseSExp (* E-exp starts with S-exp always + ; *)
    toklist
    (fun toklist' exp -> match toklist' with
       | SEMICOLON :: toklist'' -> sc toklist'' exp (* Note E ::= S; as defined, there should be nothing after semi-colon when calling original sc. original call to parse includes SEMI-COLON *)
       | _ -> raise (Error "Expected a single semicolon"))

and parseSExp (toklist: token list) (sc: (token list -> exp -> 'a)) : 'a = 
  (* There's no parenthesis in an S expression *)
  (* S-exp always starts with a P exp, thus parse P-Exp *)
  parsePExp toklist (* the parseS sc function here needs to handle what parseP "returns" *) 
    (fun toklist' exp -> match toklist' with (* pass intermediate value to continuation *)
       | PLUS::toklist'' -> parseSExp toklist'' (fun tl e2 -> sc tl (Sum (exp, e2)))
       | SUB::toklist'' -> parseSExp toklist'' (fun tl e2 -> sc tl (Minus (exp, e2)))
       | l -> sc l exp) (* next token is not part of S 
                                therefore case 3: P (i.e. exp) itself 
                        *)

and parsePExp (toklist: token list) (sc: (token list -> exp -> 'a)) : 'a =
  (*raise NotImplemented*)
  (* P-exp always starts with a A-Exp, thus parse A-Exp *)
  parseAtom toklist (* the ParseP sc function here needs to handle what parseA "returns" *)
    (fun toklist' exp -> match toklist' with
       | TIMES::toklist'' ->  parsePExp toklist'' (fun tl e2 -> sc tl (Prod(exp, e2)))
       | DIV::toklist'' ->  parsePExp toklist'' (fun tl e2 -> sc tl (Div(exp, e2)))
       | l -> sc l exp) (* Case 3: A (i.e. exp) itself *)

and parseAtom (toklist: token list) (sc: (token list -> exp -> 'a)) : 'a =
  (* raise NotImplemented *)
  (* Atomic E is either an n or (S) *)
  (* try without parenthesis first *)
  match toklist with 
  | (INT(n))::toklist' -> sc toklist' (Int(n))
  | LPAREN::toklist' -> parseSExp toklist' (* the parseA sc here handles what parsS "returns" *)
                          (fun toklist'' exp -> match toklist'' with
                             | RPAREN::toklist''' -> sc toklist''' exp 
                             | _ -> raise (Error "RPAREN expected"))
  | _ -> raise (Error "Ill-formed expression")
                             

(* parse : string -> exp *)
let parse string =
  parseExp
    (lex string)
    (fun s e -> match s with
       | [] -> e
       | _ -> raise (Error "Incomplete expression"))

(* eval : string -> int *)
let eval e = eval' (parse e)

(* ---------- Hamming Numbers ----------- *)
(* int str -> int str -> int str *)
let rec merge (s1: 'a str) (s2: 'a str) = 
  let (f,s) = if s1.hd > s2.hd then (s2,s1) else if s1.hd > s2.hd then (s1,s2) else (s1, (force s2.tl)) (* only take 1 of them if equal by skipping to next head in the other one *)
  in
  {hd = f.hd;
   tl = Susp (fun () -> merge (force f.tl) s  )} (* pass s; element not yet in stream *)
(* hamming_series : int str , create the 3 infinite series and pass to merge *)
let rec hamming_series = 
  {hd = 1 (* we want it to start from 1*);
   tl = Susp (fun () -> hamming_series)
       (*let s' = merge (times 2 s) (times 3 s) in merge s' (times 5 s)*)
  }
  




