(* ------------------------------------------------------------------------*)
(* Q 1 : Money in the bank (25 points)                                     *)
(* ------------------------------------------------------------------------*)

let new_account (p: passwd) : bank_account =
  (* Shared states *)
  let counter = ref 0 in
  let balance = ref 0 in
  let password = ref p in
  { update_passwd = (fun oldpwd newpwd -> if oldpwd = !password 
                      then (counter := 0 ; 
                            password := newpwd) (* reset counter after successful pwd change *)
                      else (counter := !counter + 1 ; raise wrong_pass)) ;
    retrieve = (fun pwd sum -> if !counter >= 3 then raise too_many_attempts
                 else if pwd = !password 
                 then (counter := 0 ; if !balance - sum >= 0 
                       then balance := !balance - sum 
                       else raise no_money)
                 else (counter := !counter + 1 ; raise wrong_pass)) ;
    deposit = (fun pwd sum -> if !counter >= 3 then raise too_many_attempts
                else if pwd = !password 
                then (counter := 0 ; balance := !balance + sum)
                else (counter := !counter +1 ; raise wrong_pass)) ;
    print_balance = (fun pwd -> if !counter >= 3 then raise too_many_attempts
                      else if pwd = !password
                      then (counter := 0 ; !balance)
                      else (counter := !counter + 1 ; raise wrong_pass)) }
;;


(* ------------------------------------------------------------------------*)
(* Q 2 : Memoization (75 points)                                           *)
(* ------------------------------------------------------------------------*)

(* Q 2.1 : Counting how many function calls are made *)

let rec fib_I (n: int) : fib_result =
  (* shared states? *) 
  let count = ref 0 in
  let rec fib' num =
    if num = 0 then (count := !count +1 ; 0)
    else (if num = 1 
          then (count := !count +1 ; 1) 
          else (count := !count +1 ; fib' (num-2) + fib' (num-1))) in 
  { num_rec = !count ;
    result = fib' n } 
;;


(* Q 2.2 : Memoization with a global store *)

let fib_memo (n: int) : int =
  let rec fib n =
    (* Determine whether value already in hashtable *)
    match Hashtbl.find_opt store n with
    | Some(fibn) -> fibn
    | None -> 
        let fibn = (if n = 0 then 0
                    else (if n = 1 then 1 else fib (n-2) + fib (n-1))) in
        (Hashtbl.add store n fibn ; fibn)
  in
  fib n
;;


(* Q 2.3 : General memoization function *)
(* the function input takes 2 arguments, one being a function and returns another function 'a -> 'b *)
let memo (f: (('a -> 'b) -> 'a -> 'b)) (stats: stats) : ('a -> 'b) =
  (* first define the hash table *)  
  let loc_store : ('a, 'b) Hashtbl.t = Hashtbl.create 1000 in
  (* input 'a *)
  fun x ->
  (* turn f into "real" function of type int -> int using g *) 
    let rec g a =
      match Hashtbl.find_opt loc_store a with
      | Some(res) -> (stats.lkp := !(stats.lkp) + 1 ; res)
      | None ->
          (stats.entries := !(stats.entries) + 1 ; 
           let b = f g a in Hashtbl.add loc_store a b ; b) 
(* Note that function f will pass a to g, initiating the recursive call *)
    in
    g x; 
;;


(* Q 2.4 : Using memo to efficiently compute the Fibonacci number *)
(* We also accept let fibM = raise NotImplemented ;; *)
(* int -> (int * stats) *)
let fibM =
  (* lift stats and memof out of fun n to force their computation
  prior to function body. Thus we can save the stats and memof 
  for every instance/call of fibM *)
  (* Create stats record for this function *)
  let stats = 
    let e1 = ref 0 in
    let e2 = ref 0 in
    { entries = e1;
      lkp = e2 } in
  (* Create memo for this function *)
  let memof = memo (fun g x -> if x = 0 then 0
                     else (if x = 1 then 1 else g (x-2) + g (x-1))) stats 
  in 
  (* function body *) (* call memof in here with n *)
  fun n -> (memof n, stats) 
  
;;
