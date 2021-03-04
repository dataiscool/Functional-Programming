(* Q0  : Get familiar with the external syntax of MiniML *)
let parse_tests : (string * (string, exp) either) list = [
    (* Provide your tests for the parser *)
  ("1;", Right (Int 1));
  ("let val try = (fn x => if x = 1 then 1 else 0) : int -> int in try 0 end;" , Right(Let([Val(Anno(Fn("x", None, If(Primop(Equals, [Var "x"; Int 1]), Int 1, Int 0)), TArrow(TInt, TInt)), "try")], Apply(Var "try", Int 0))))
                                                                           
]

let free_vars_tests : (exp * name list) list = [
  (Int 10, []);
  (Var "x", ["x"]);
  (Primop(Plus, [Var "y"; Var "x"]), ["y";"x"]);
  (Let ([Val (Var "y", "x")], Primop (Plus, [Var "x"; Int 10])), ["y"]);
  (Let([Valtuple(Tuple [Int 1; Tuple [Int 2; Int 3]], ["x"; "y"; "z"])], Primop(Plus, [Var "x"; Var "y"; Var "z"])), []);
  (Let ([Val (Var "y", "x")], Primop (Plus, [Var "x"; Int 10])), ["y"]);
  (Let ([ Val (Rec (" fact ", TArrow (TInt , TInt ), Fn ("x", Some TInt , If ( Primop (Equals , [Var "x"; Int 0]) , Int 1, Primop (Times , [Var "x"; Apply (Var " fact ",Primop (Minus ,[Var "x"; Int 1]))]))))," fact ")],Apply (Var " fact ", Int 5)), []) ;
  (Let
     ([Val
         (Anno
            (Fn
               ("y", None,
                Primop (Plus, [Var "y"; Primop (Times, [Var "x"; Int 2])])),
             TArrow (TInt, TInt)),
          "func")],
      Apply (Var "func", Primop (Plus, [Var "y"; Int 1]))), ["x";"y"]);
  (Let ([ByName (Primop(Plus, [Var "a"; Var "b"]), "x"); Val (Var "c", "y")],
        Primop (Times, [Var "x"; Var "y"])), ["a";"b";"c"]);
  (Let ([Val (Var "x", "x")], Primop (Times, [Int 2; Var "x"])), ["x"]);
  (Let ([Val (Int 3, "x"); Valtuple (Tuple [Var "x"; Int 2], ["y1"; "y2"])],
        Primop (Plus, [Var "x"; Var "y1"])), [])
]

(* Q1  : Find the free variables in an expression *)
let rec free_vars (e : exp) : name list = match e with
  | Var y -> [y]
  | Int n -> []
  | Bool b -> []
  | If(e, e1, e2) ->
      union (free_vars e) (union (free_vars e1) (free_vars e2))
  | Primop (po, args) ->
     (* recall: FV (e1 op e2) = union (FV e1 , FV e2)) *)
      union_list (List.map free_vars args)
        (* List.fold_right (fun e1 fv -> union (free_vars e1) fv) args [] *)
  | Tuple(expL) ->
      (* List.fold_right (fun e1 fv -> union (free_vars e1) fv) expL []*)
      union_list (List.map free_vars expL)
  | Fn(name, tpOpt, e) -> delete [name] (free_vars e) (* name is bound in function body *)
  | Rec(name, typ, e) -> delete [name] (free_vars e) 
(* Note that dec is a list of declarations, perform free vars on each element *)
  | Let (decL, e2) -> 
      let getName dec = match dec with (* Keep track of bound vars *) 
        | Val(_,name) | ByName(_,name) -> [name]
        | Valtuple(_,nameL) -> nameL 
      in 
      let rec e1_fv decList names = match decList with (* Collect FV in e1 *)
        | [] -> [] 
        | (Val(e,_) as d)::l 
        | (Valtuple(e,_) as d)::l (* remove bindings to previous declarations *)
        | (ByName(e,_) as d)::l -> union (free_vars e) (delete ((getName d)@names) (e1_fv l ((getName d)@names))) 
      in 
      union (e1_fv decL []) (delete (union_list (List.map getName decL)) (free_vars e2))
  | Apply(fn_e,e) -> union (free_vars fn_e) (free_vars e)
  | Anno(e, tp) -> free_vars e


(* n-ary tuples: Valtuple(Tuple([Int 1; Tuple([Int 2; Int 3])]), ["x"; "y"; "z"])
equivalent to (x,y,z) = (1,2,3) *) 

let unused_vars_tests : (exp * name list) list = [
  (Let ([Val (Int 3, "x")], Int 4), ["x"]);
  (Let ([Val (Bool true, "x")],
        Let ([Val (Int 4, "y")], Primop (Plus, [Var "x"; Int 5]))), ["y"]);
  (Let ([Val (Int 3, "x")],
        Let ([Val (Int 4, "x")], Primop (Plus, [Var "x"; Var "x"]))), ["x"]);
  (Let
     ([Val (Rec ("test", TArrow (TInt, TInt), Fn ("x", Some TInt, Int 3)),
            "test")],
      Int 4), ["x";"test"]);
  (Let
     ([Val
         (Anno
            (Fn
               ("y", None,
                Primop (Plus, [Var "y"; Primop (Times, [Var "x"; Int 2])])),
             TArrow (TInt, TInt)),
          "func")],
      Int 3), ["func"]);
  (Let
     ([Val
         (Anno
            (Fn
               ("y", None,
                Primop (Plus, [Var "y"; Primop (Times, [Var "x"; Int 2])])),
             TArrow (TInt, TInt)),
          "func")],
      Apply (Var "func", Primop (Plus, [Var "y"; Int 1]))), []);
  (Primop (Plus,
           [Let ([Val (Int 2, "x")], Int 4); Let ([Val (Int 2, "y")], Int 4)]), ["x";"y"]);
  (Let
     ([Valtuple (Tuple [Int 5; Let ([Val (Var "y", "a")], Var "z")],
                 ["z"; "x"; "c"])],
      Int 10), ["a"; "z"; "x"; "c"])
]

(* Q2  : Check variables are in use *)
let rec unused_vars (e : exp) : name list = match e with
  (* For every binding construct (Fn, Rec, Let)
     Collect all bound variables introduced by an e1
     Check whether these bound variables occur free in the body
     If they don't, then unused *) 
  | Fn(name, tpOpt, e) -> if member name (free_vars e) then unused_vars e else union [name] (unused_vars e)
  | Rec(name, typ, e) -> (*if member name (free_vars e) then*) unused_vars e (*else union [name] (unused_vars e)*)
(* Note that dec is a list of declarations, perform free vars on each element *)
  | Let (decL, e2) -> 
      (* Collect unused vars in e1 *)
      let rec e1_uv decList = match decList with
        | [] -> []
        | Val(e,_)::l | ByName(e,_)::l | Valtuple(e,_)::l -> union (unused_vars e) (e1_uv l) 
      in 
      let rec collect_fvs decL = match decL with (* collect free vars of each individual nested Let's *)
        | [] -> []
        | Val(e, _)::l | ByName(e,_)::l -> (free_vars e)@(collect_fvs l)
        | Valtuple(e,nl)::l -> (free_vars e)@(collect_fvs l)
      in
      let rec let_uv dec_list = match dec_list with
        | [] -> []
          (*union (delete nameL (delete (name d) (free_vars e2))) (let_uv l ((name d)@nameL) ) *)
        | (Valtuple(e,nl))::l -> 
            let rec nl_traverse l' = match l' with
              | [] -> []
              | n::l' -> if member n (collect_fvs l) || member n (free_vars e2) 
                  then union (nl_traverse l') (let_uv l) else union (nl_traverse l') (union [n] (let_uv l))
            in
            (nl_traverse nl) 
        | (ByName(e,n))::l | (Val(e,n))::l -> if member n (collect_fvs l) || member n (free_vars e2)
            then let_uv l else union [n] (let_uv l)
      (*let rec let_uv' name_list = match name_list with
        | [] -> []
        | name::remL -> if member name (free_vars e2) then (let_uv remL) else union [name] (let_uv remL) *)
      in
      union (e1_uv decL) (union (let_uv decL) (unused_vars e2)) 
  | If(e, e1, e2) ->
      union (unused_vars e) (union (unused_vars e1) (unused_vars e2)) 
  | Apply(fn_e,e) -> union (unused_vars fn_e) (unused_vars e)
  | Anno(e, tp) -> unused_vars e 
  | Tuple expList -> 
      let rec tup_uv elist = match elist with
        | [] -> []
        | e::rem -> union (unused_vars e) (tup_uv rem)
      in tup_uv expList
  | Primop (_, expList) -> 
      let rec prim_uv elist = match elist with
        | [] -> []
        | e::rem -> union (unused_vars e) (prim_uv rem)
      in prim_uv expList
  | _ -> []

let subst_tests : (((exp * name) * exp) * exp) list = [
  (((Int 5, "x"), (If ( Bool ( true ), Var "x", Var "y"))), If ( Bool true , Int 5, Var "y"));
  (((Var "y", "x"), (Let ([Val (Var "x", "y")], Primop (Plus, [Var "y"; Primop (Plus, [Var "x"; Var "y"])])))), Let ([Val (Var "y", "1y")],
                                                                                                                     Primop (Plus, [Var "1y"; Primop (Plus, [Var "y"; Var "1y"])])));
  (((Var "y", "x") ,(Let ([Val (Primop (Times, [Var "x"; Int 2]), "y")],
                          Primop (Times, [Var "x"; Var "y"])))), Let ([Val (Primop (Times, [Var "y"; Int 2]), "1y")],
                                                                      Primop (Times, [Var "y"; Var "1y"])));
  (((Primop(Plus, [Var "y"; Var "q"]), "x"), (Let ([Val (Primop (Times, [Var "x"; Int 2]), "y")],
                                                   Let ([Val (Primop (Minus, [Var "y"; Int 1]), "k")],
                                                        Primop (Times, [Var "k"; Var "x"]))))), (Let([Val (Primop (Times, [Primop (Plus, [Var "y"; Var "q"]); Int 2]), "1y")],
                                                                                                     Let ([Val (Primop (Minus, [Var "1y"; Int 1]), "k")],
                                                                                                          Primop (Times, [Var "k"; Primop (Plus, [Var "y"; Var "q"])])))));
]

(* Q3  : Substitute a variable *)
let rec subst ((e', x) : exp * name) (e : exp) : exp =
  match e with
  | Var y ->
      if x = y then
        e'
      else
        Var y

  | Int _ | Bool _ -> e
  | Primop (po, es) -> Primop (po, List.map (subst (e', x)) es)
  | If (e1, e2, e3) -> If (subst (e', x) e1, subst (e', x) e2, subst (e', x) e3)
  | Tuple es -> Tuple (List.map (subst (e', x)) es)
  | Anno (e, t) -> Anno (subst (e', x) e, t)

  | Let (ds, e2) -> 
      (* Logic: Separated in two parts: sub in e1 first, if x = y, then sub only in e1 
      If x =/= y, but y is not a member of the FVs of e', then can still sub directly
      If y IS a member (x=/=y), then rename to avoid capture. 
      For e2: y in e2 are bound to let, thus if x=y then dont sub. Build up e2subbed tail-recursively, renaming to avoid capture if necessary *)
      if ds = [] then subst (e',x) e2 else
        let rec sub_e1 (decs) = match decs with (* note if x = y, then only e1 subbed, and y is passed to y' *)
          | [] -> []
          | Val(e1,y)::l -> (* Note that only the y in e1 is free, thus if x=y, only sub in e1 *)
              let e1' = subst (e',x) e1 in (* e1 only contains FVs; Recall even if same name still FV *)
              if x = y || not (member y (free_vars e')) (* if x = y or if not but y is not member of FVs, then sub directly (dont need to rename) *)
              then (Val (e1', y))::(sub_e1 l) else
                let y' = fresh_var y in
                (Val (e1', y'))::(sub_e1 l) 
          | ByName(e1,y)::l -> 
              let e1' = subst (e',x) e1 in
              if x = y || not (member y (free_vars e')) 
              then (ByName (e1', y))::(sub_e1 l) else
                let y' = fresh_var y in
                (ByName (e1', y'))::(sub_e1 l)
          | Valtuple(e1,ys)::l -> 
              let e1' = subst (e',x) e1 in 
              let rec ystraverse ys' = match ys' with
                |[]->[]
                | y::ys -> if x = y || not (member y (free_vars e')) then
                      y::(ystraverse ys) else
                      let y' = fresh_var y in
                      y'::(ystraverse ys)
              in (Valtuple (e1', (ystraverse ys)))::(sub_e1 l)
        in
        let rec boundNames decs names = match decs with
          | [] -> names
          | Val(_,y)::l | ByName(_,y)::l -> boundNames l (y::names)
          | Valtuple(_,ys)::l -> boundNames l (ys@names)
        in
        let bndNames = boundNames ds [] 
        in
        let rec sub_e2 (decs, e2subbed) = match decs with
          | [] -> e2subbed
          | Val(e1, y)::l  | ByName(e1, y)::l -> if x = y || member x bndNames then sub_e2 (l, e2subbed) else
              if member y (free_vars e') then
                let y' = fresh_var y in 
         (* else x=/=y, then sub in both e1 and e2. Start by making sure we avoid capture *)
                let e2' = subst (Var y', y) e2subbed (* replace y var in e2 by y' to avoid capture *)
                in  sub_e2 (l, subst (e',x) e2') (* sub in e2 after renaming and build tail-recursively *)
              else (* no danger of capture, sub directly *)
                sub_e2 (l, subst (e',x) e2subbed)
          | Valtuple(e1, ys)::l -> 
              let rec ystraverse ys' e2subbed' = match ys' with
                | [] -> e2subbed' 
                | y::ys -> if x = y || member x bndNames then ystraverse ys e2subbed else 
                    if member y (free_vars e') then
                      let y' = fresh_var y in 
                      let e2' = subst (Var y', y) e2subbed (* replace y var in e2 by y' to avoid capture *)
                      in ystraverse ys (subst (e',x) e2') (* sub in e2 after renaming and build tail-recursively *)
                    else ystraverse ys (subst (e',x) e2subbed)
              in sub_e2 (l, ystraverse ys e2subbed)
        in (* just in case it is necessary *)
        Let((reset_ctr(); sub_e1 ds), (reset_ctr(); sub_e2 (ds, e2)))
(* reset counter so that e2 fresh vars are the same as in e1 when renamed *)
  
  | Apply (e1, e2) -> Apply (subst (e',x) e1, subst (e',x) e2) 
  | (Fn (y, t, e)) as f -> 
      if member x (free_vars f) then (* Replace free occurrences of x only. Rename bound variable of fn if need to avoid capture *)
        if member y (free_vars e') then (* check if there will be capture *)
          let y' = fresh_var y in (* rename bound variable and replace it in e *)
          let e2' = subst (Var y', y) e in
          Fn (y', t, subst (e',x) e2') 
        else Fn (y, t, subst (e',x) e) (* Else no wrong capture *)
      else Fn (y, t, e)
  | Rec (y, t, e) -> Rec (y, t, subst (e',x) e)


let eval_tests : (exp * exp) list = [
  ((Let
      ([Val
          (Rec ("repeat",
                TArrow (TInt, TArrow (TArrow (TInt, TInt), TArrow (TInt, TInt))),
                Fn
                  ("n", Some TInt,
                   Fn
                     ("f", Some (TArrow (TInt, TInt)),
                      Fn
                        ("x", Some TInt,
                         If (Primop (Equals, [Var "n"; Int 0]), Var "x",
                             Apply
                               (Apply
                                  (Apply (Var "repeat", Primop (Minus, [Var "n"; Int 1])),
                                   Var "f"),
                                Apply (Var "f", Var "x"))))))),
           "repeat")],
       Apply
         (Apply (Apply (Var "repeat", Int 4),
                 Fn ("z", Some TInt, Primop (Times, [Var "z"; Int 2]))),
          Int 100))), Int 1600);
  ((Let
      ([Valtuple
          (Tuple [Primop (Plus, [Int 2; Int 1]); Primop (Times, [Int 2; Int 50])],
           ["x"; "y"])],
       Primop (Times, [Primop (Times, [Var "x"; Var "x"]); Var "y"]))), Int 900);
  ((Let
      ([Val
          (Rec ("fact", TArrow (TInt, TInt),
                Fn
                  ("x", Some TInt,
                   If (Primop (Equals, [Var "x"; Int 0]), Int 1,
                       Primop (Times,
                               [Var "x"; Apply (Var "fact", Primop (Minus, [Var "x"; Int 1]))])))),
           "fact")],
       Apply (Var "fact", Int 5))), Int 120)
]

(* Q4  : Evaluate an expression in big-step *)
let rec eval : exp -> exp =
  (* do not change the code from here *)
  let bigstep_depth = ref 0 in
  fun e ->
    if !debug >= 1 then
      print_endline
        (String.make (!bigstep_depth) ' '
         ^ "eval (" ^ Print.exp_to_string e ^ ")\n");
    incr bigstep_depth;
  (* to here *)
    let result =
      match e with
      | Int _ | Bool _ -> e
      | Tuple es -> Tuple (List.map eval es)
      | If (e1, e2, e3) ->
          begin match eval e1 with
            | Bool b ->
                if b then
                  eval e2
                else
                  eval e3
            | _ -> stuck "Condition for if expression should be of the type bool"
          end
      | Anno (e, _) -> eval e     (* types are ignored in evaluation *)
      | Var x -> stuck ("Free variable \"" ^ x ^ "\" during evaluation")

      | Fn (x, t, e) -> Fn (x, t, e)
      | Apply (e1, e2) -> let f = eval e1 in 
          begin match f with 
            | Fn (x,t,e) -> eval (subst (eval e2, x) e) 
            | _ -> stuck "Expected function as first argument"
          end
      | Rec (f, t, e) -> eval (subst (Rec (f,t,e), f) e)

      | Primop (And, es) ->
          let rec booland l = match l with 
            | [] -> Bool true
            | (Bool false)::l -> Bool false
            | (Bool true)::l -> booland l
            | _ -> stuck "Expected expressions to be of type bool"
          in booland (List.map eval es)  
      | Primop (Or, es) ->
          let rec boolor l = match l with
            | [] -> Bool false
            | (Bool true)::l -> Bool true
            | (Bool false)::l -> boolor l
            | _ -> stuck "Expected expressions to be of type bool" 
          in boolor (List.map eval es)
      | Primop (op, es) ->
          let vs = List.map eval es in
          begin match eval_op op vs with
            | None -> stuck "Bad arguments to primitive operation"
            | Some v -> v
          end

      | Let (ds, e) -> 
          begin match ds with 
            | [] -> eval e (* NO-DECS*)
            | Val(e1,x)::decs -> eval (subst (eval e1, x) (Let(decs, e))) 
            | ByName(e1,x)::decs -> eval (subst (e1, x) (Let(decs, e)))
            | Valtuple(e1, xl)::decs -> 
                let vals = match eval e1 with (* Want to combine like in rules and then sub one by one *)
                  | Tuple(exps) -> exps
                  | _ -> stuck "expected a Tuple construction" 
                in 
                let sublist = List.combine vals xl in (* combine into (v,x) tuples *)
                let rec sub_in l letsubbed = match l with (* Sub in e *)
                  | [] -> letsubbed
                  | (v,x)::l -> sub_in l (subst (v,x) letsubbed)
                in
                eval (sub_in sublist (Let(decs, e))) 
          end 
    in
  (* do not change the code from here *)
    decr bigstep_depth;
    if !debug >= 1 then
      print_endline
        (String.make (!bigstep_depth) ' '
         ^ "result of eval (" ^ Print.exp_to_string e ^ ") = "
         ^ Print.exp_to_string result ^ "\n");
  (* to here *)
    result


let infer_tests : ((context * exp) * typ) list = [
  ((Ctx([]), (Let ([Val (Bool true, "x")],
                   Let ([Val (Int 1, "x")], Primop (Plus, [Var "x"; Int 5]))))), TInt);
  ((Ctx([]), Let ([Val (Bool true, "x"); Val (Int 3, "x")],
                  Primop (Times, [Int 2; Var "x"]))), TInt); 
  ((Ctx([]), (Let
   ([Val
      (Rec ("apply", TArrow (TArrow (TInt, TInt), TArrow (TInt, TInt)),
        Fn
         ("f", Some (TArrow (TInt, TInt)),
          Fn ("x", Some TInt, Apply (Var "f", Var "x")))),
      "apply")],
   Apply
    (Apply (Var "apply", Fn ("x", None, Primop (Times, [Var "x"; Int 3]))),
    Int 100)))), TInt);
  ((Ctx([]),  (Let
   ([Val
      (Rec ("fact", TArrow (TInt, TInt),
        Fn
         ("x", Some TInt,
          If (Primop (Equals, [Var "x"; Int 0]), Int 1,
           Primop (Times,
            [Var "x"; Apply (Var "fact", Primop (Minus, [Var "x"; Int 1]))])))),
      "fact")],
   Apply (Var "fact", Int 5)))), TInt); 
]

(* Q5  : Type an expression *)
(* Q7* : Implement the argument type inference
         For this question, move this function below the "unify". *)
(* let rec typ2str t = match t with
  | TInt -> "Int"
  | TBool -> "Bool"
  | TArrow (tp1, tp2) -> (typ2str tp1 ^ "->" ^ typ2str tp2)
  | TProduct (tpl) -> let rec getTp l str = match tpl with
      | [] -> str
      | tp::tps -> getTp tps (str ^ typ2str tp) 
      in getTp tpl ""
  | TVar (opt) -> raise NotImplemented


                    let rec infer (ctx : context) (e : exp) : typ = match e with
                        | Var x -> begin try ctx_lookup ctx x 
                            with NotFound -> type_fail ("Cannot find \"" ^x^ "\" in context") end
                        | Int _ -> TInt
                        | Bool _ -> TBool
                        | If (e', e1, e2) -> 
                            begin match infer ctx e' with 
                              | TBool -> let t1 = infer ctx e1 in
                                  let t2 = infer ctx e2 in
                                  if t1 = t2 then t1 
                                  else type_fail ("Expected " ^ typ2str t1 ^ " - Inferred " ^typ2str t2)
                              | t -> type_fail ("Expected Bool\nInferred " ^ typ2str t)
                            end
      
                        | Fn (x, tp, e') -> (* extend context for x *) 
                            begin match tp with 
                              | Some typ -> TArrow(typ, infer (extend ctx (x, typ)) e')
                              | None -> type_fail "Missing type annotation" (* But what if using Anno? *)
                            end
                        | Rec (f, tp, e') -> infer (extend ctx (f, tp)) e'
                        | Tuple es -> TProduct (List.map (infer ctx) es)
                        | Apply (e1, e2) -> 
                            begin match infer ctx e1 with
                              | TArrow (t1, t') -> let t2 = infer ctx e2 in
            if t1 = t2 then t' 
            else type_fail ("This expression has type" ^ typ2str t2 ^
                            " but an expression was expected of type" ^ typ2str t1)
        | t -> type_fail "Expected function"
      end
      
  | Let (decs, e2) -> 
  (* td_by returns a list of contexts *)
      (* Note that contexts add to front of context list, thus "overshadow"
      previous contexts, no need to rename...*)
      let t_by ctx dec = match dec with
        | Val (e1, x) | ByName (e1, x) -> [(x, infer ctx e1)]
        | Valtuple (e1, xs) -> 
            let t1s = infer ctx e1 in
            begin match t1s with 
              | TProduct typList -> begin try List.combine xs typList
                  with Invalid_argument _ -> type_fail "Expression bound to an n-tuple is expected to n-tuple types" end
              | _ -> type_fail "Expected a tuple"
            end
      in (* tdecs returns a gamma1 extend gamma2 *)
      let rec tdecs ctx decls = match decls with
        | [] -> ctx
        | dec1::decs -> let gamma1 = t_by ctx dec1 in
            tdecs (extend_list ctx gamma1) decs 
      in
      let ctx = tdecs ctx decs in
      infer ctx e2
  
  | Anno (e',tp) -> 
      let tp' =  infer ctx e' in
      if tp' = tp then tp else type_fail ("Expression of type " ^ typ2str tp' ^" - Expected" ^ typ2str tp)
          
  | Primop (op, es) ->  
  (* Note that recursion will take care of cases where there are multiple
  expressions in es *)
      let primopType p = match p with
        | Equals -> ([TInt; TInt], TBool)
        | NotEquals -> ([TInt; TInt], TBool)
        | LessThan -> ([TInt; TInt], TBool)
        | LessEqual -> ([TInt; TInt], TBool)
        | GreaterThan -> ([TInt; TInt], TBool)
        | GreaterEqual -> ([TInt; TInt], TBool)
        | And -> ([TBool; TBool], TBool)
        | Or -> ([TBool; TBool], TBool)
        | Plus -> ([TInt; TInt], TInt)
        | Minus -> ([TInt; TInt], TInt)
        | Times -> ([TInt; TInt], TInt)
        | Div -> ([TInt; TInt], TInt)
        | Negate -> ([TInt], TInt)
      in
      let (expected_types, resultType) = primopType op in
      let inferred_types = List.map (infer ctx) es in
      let rec compare typList1 typList2 = match typList1, typList2 with
        | [], [] -> resultType
        | t1::t1l, t2::t2l ->
            if t1 = t2 then compare t1l t2l 
            else type_fail ("Expected " ^ typ2str t1 ^ " - Inferred " ^ typ2str t2)
        | _, _ -> type_fail "Error: Primitve operator used with incorrect number of arguments"  
      in compare expected_types inferred_types
*)

let unify_tests : ((typ * typ) * unit) list = [
  ((TVar(ref (Some(TInt))), TInt), ());
  ((TVar(ref (Some(TVar(ref (Some(TBool)))))), TBool), ());
  ((TVar(ref (Some(Tvar(ref (Some(TArrow(TVar(ref None), TInt)))))))), TArrow(TInt, TVar(ref None)), ());
  ((TInt, TArrow(TVar(ref None), TVar(ref None))), ());
  (((TArrow(TInt, TInt)), (TArrow(TInt, TVar(ref None)))),());
  (((TProduct[TInt; TBool]), (TProduct[TInt; TVar(ref None)])), ());
  (((TArrow(TVar(ref None), TProduct[TVar(ref None); TVar(ref None)])), (TArrow(TArrow(TInt, TVar(ref None)), TArrow(TVar(ref None), TVar(ref None))))), ());
]

(* find the next function for Q5 *)
(* Q6  : Unify two types *)
(* Unfold TVar's until reach a non-Tvar or a variable with None (no assignment) *)
(* typ option ref -> typ option ref *)
let rec unfold_tvar a = match !a with
  | Some(TVar(a')) -> unfold_tvar a'
  | None -> a 
  | Some(tp) -> a (* non-TVar, since TVar is already matched above *)
  
(* typ option ref -> typ -> bool. Determine whether a is a FV in T
Note that a is unfolded, already a None, thus only need check address 
Note t can still be a TVar even if it came from unfolding, 
because there might be TVars embedded in TArrow or TProduct *)
let rec free_tvars a t = match t with
  | TVar(a') -> a == unfold_tvar a'
  | TArrow (tp1, tp2) -> begin match tp1, tp2 with
      | TVar(a'), TVar(b) -> a == unfold_tvar a' && a == unfold_tvar b
      | TVar(a'), tp' | tp', TVar(a') -> a == unfold_tvar a' && free_tvars a tp' 
      | tp1', tp2' -> free_tvars a tp1' && free_tvars a tp2'
    end
  | TProduct(typs) -> 
      let rec traverse l = match typs with
        | [] -> false
        | tp::tps -> match tp with 
          | TVar(a') -> a == unfold_tvar a' (* returns false if not same *)
          | tp' -> free_tvars a tp' || traverse tps (* either can be true *)
      in traverse typs
  | TInt -> false
  | TBool -> false
  
    (*let substype a b =  (* Use to mutually update TVars. takes 2 option typ ref *)
       if a = b then ()
       else (* a <> b *) 
       if !a = None then a := !b else b := !a  *) 
  
let rec unify (ty1 : typ) (ty2 : typ) : unit = match ty1, ty2 with
  | TInt, TInt -> ()
  | TBool, TBool -> ()
  | TArrow(tp1, tp2), TArrow(tp1', tp2') -> (unify tp1 tp1'; unify tp2 tp2') 
  | TProduct(tpList1), TProduct(tpList2) -> 
      let rec traverse l1 l2 = match l1, l2 with
        | [], [] -> ()
        | tp1::tp1s, tp2::tp2s -> (unify tp1 tp2; traverse tp1s tp2s) 
        | _, _ -> type_fail "Not unifyable" (* has to be same length *)
      in 
      traverse tpList1 tpList2
  | TVar(a), TVar(b) -> if a = b then () else (* as in notes *)
        let a' = unfold_tvar a in
        let b' = unfold_tvar b in 
        begin match !a', !b' with
          | None, None -> () (* a = a *)
          | None, Some(tp') -> 
              if free_tvars a' tp' 
              then type_fail "Not unifyable" else a' := !b' (* a = T, a E/ FV(T) *)
          | Some(tp'), None -> 
              if free_tvars b' tp'
              then type_fail "Not unifyable" else b' := !a' (* T = a *)
          | Some(tp1'), Some(tp2') -> unify tp1' tp2'  (* e.g. two types TInt and TInt *)
        end
  | TVar(a), tp' | tp', TVar (a)-> (* Case where we have a and T *) 
      let a' = unfold_tvar a in 
      begin match !a' with
        | None ->  if free_tvars a' tp' then type_fail "Not unifyable" 
            else a := Some tp'
        | Some(tp1) -> unify tp1 tp' 
      end 
  | _, _ -> type_fail "Not unifyable" 
              
(* Bonus question *)

let rec typ2str t = match t with
  | TInt -> "Int"
  | TBool -> "Bool"
  | TArrow (tp1, tp2) -> (typ2str tp1 ^ "->" ^ typ2str tp2)
  | TProduct (tpl) -> let rec getTp l str = match tpl with
      | [] -> str
      | tp::tps -> getTp tps (str ^ typ2str tp) 
      in getTp tpl ""
  | TVar (a) -> match !(unfold_tvar a) with
    | Some(typ) -> typ2str typ
    | None -> "Tvar"
(*| TVar (opt) -> match opt with
                | Some tp -> typ2str tp
              | None -> 
            NO need to take care of this now*)

let rec infer (ctx : context) (e : exp) : typ = match e with
  | Var x -> begin try ctx_lookup ctx x 
      with NotFound -> type_fail ("Cannot find \"" ^x^ "\" in context") end
  | Int _ -> TInt
  | Bool _ -> TBool
  | If (e', e1, e2) -> 
      begin match infer ctx e' with 
        | TBool -> let t1 = infer ctx e1 in
            let t2 = infer ctx e2 in 
            begin try (unify t1 t2; t1) 
              with TypeError _ -> type_fail ("Expected " ^ typ2str t1 ^ " - Inferred " ^typ2str t2) end
        | t -> type_fail ("Expected Bool - Inferred " ^ typ2str t)
      end
      
  | Fn (x, tp, e') -> (* extend context for x *) 
      begin match tp with 
        | Some typ -> TArrow(typ, infer (extend ctx (x, typ)) e')
        | None ->  let tv = fresh_tvar() in TArrow(tv, infer (extend ctx (x, tv)) e')
      end
  | Rec (f, tp, e') -> infer (extend ctx (f, tp)) e' 
  | Tuple es -> TProduct (List.map (infer ctx) es)
  | Apply (e1, e2) -> 
      begin match infer ctx e1 with
        | TArrow (t1, t') -> let t2 = infer ctx e2 in
            begin try (unify t1 t2; t') (* Now we can drop annotation; if we can unify t1 and t2 which is the unannotated function, then can infer type of result *)
              with TypeError _ ->
                type_fail ("This expression has type" ^ typ2str t2 ^
                           " but an expression was expected of type" ^ typ2str t1) end
        | t -> type_fail "Expected function"
      end
      
  | Let (decs, e2) -> 
  (* td_by returns a list of contexts *)
      (* Note that contexts add to front of context list, thus "overshadow"
      previous contexts, no need to rename...*)
      let t_by ctx dec = match dec with
        | Val (e1, x) | ByName (e1, x) -> [(x, infer ctx e1)]
        | Valtuple (e1, xs) -> 
            let t1s = infer ctx e1 in
            begin match t1s with 
              | TProduct typList -> begin try List.combine xs typList
                  with Invalid_argument _ -> type_fail "Expression bound to an n-tuple is expected to n-tuple types" end
              | _ -> type_fail "Expected a tuple"
            end
      in (* tdecs returns a gamma1 extend gamma2 *)
      let rec tdecs ctx decls = match decls with
        | [] -> ctx
        | dec1::decs -> let gamma1 = t_by ctx dec1 in
            tdecs (extend_list ctx gamma1) decs 
      in
      let ctx = tdecs ctx decs in
      infer ctx e2
  
  | Anno (e',tp) -> 
      let tp' =  infer ctx e' in
      begin try (unify tp' tp; tp) 
        with TypeError _  -> 
          type_fail ("Expression of type " ^ typ2str tp' ^" - Expected" ^ typ2str tp) end
          
  | Primop (op, es) ->  
  (* Note that recursion will take care of cases where there are multiple
  expressions in es *)
      let primopType p = match p with
        | Equals -> ([TInt; TInt], TBool)
        | NotEquals -> ([TInt; TInt], TBool)
        | LessThan -> ([TInt; TInt], TBool)
        | LessEqual -> ([TInt; TInt], TBool)
        | GreaterThan -> ([TInt; TInt], TBool)
        | GreaterEqual -> ([TInt; TInt], TBool)
        | And -> ([TBool; TBool], TBool)
        | Or -> ([TBool; TBool], TBool)
        | Plus -> ([TInt; TInt], TInt)
        | Minus -> ([TInt; TInt], TInt)
        | Times -> ([TInt; TInt], TInt)
        | Div -> ([TInt; TInt], TInt)
        | Negate -> ([TInt], TInt)
      in
      let (expected_types, resultType) = primopType op in
      let inferred_types = List.map (infer ctx) es in
      let rec compare typList1 typList2 = match typList1, typList2 with
        | [], [] -> resultType
        | t1::t1l, t2::t2l ->
            begin try (unify t1 t2; compare t1l t2l) 
              with TypeError _ -> 
                type_fail ("Expected " ^ typ2str t1 ^ " - Inferred " ^ typ2str t2) end
        | _, _ -> type_fail "Error: Primitve operator used with incorrect number of arguments"  
      in compare expected_types inferred_types              

(* Now you can play with the language that you've implemented! *)
let execute (s: string) : unit =
  match P.parse s with
  | Left s -> print_endline ("parsing failed: " ^ s)
  | Right e ->
      try
       (* first we type check the program *)
        ignore (infer (Ctx []) e);
        let result = eval e in
        print_endline ("program is evaluated to: " ^ Print.exp_to_string result)
      with
      | NotImplemented -> print_endline "code is not fully implemented"
      | Stuck s -> print_endline ("evaluation got stuck: " ^ s)
      | NotFound -> print_endline "variable lookup failed"
      | TypeError s -> print_endline ("type error: " ^ s)
      | e -> print_endline ("unknown failure: " ^ Printexc.to_string e)
               

  

(************************************************************
 *             Do not change these functions.               *
 *               They are needed for tests.                 *
 ************************************************************)
let list_to_string el_to_string l : string =
  List.fold_left
    begin fun acc el ->
      if acc = "" then
        el_to_string el
      else
        acc ^ "; " ^ el_to_string el
    end
    ""
    l
  |> fun str -> "[" ^ str ^ "]"

let run_test name f ts stringify : unit =
  List.iteri
    begin fun idx (input, expected_output) ->
      try
        let output = f input in
        if output <> expected_output then
          begin
            print_string (name ^ " test #" ^ string_of_int idx ^ " failed\n");
            print_string (stringify output ^ " <> " ^ stringify expected_output)
          end
      with
      | exn ->
          print_string (name ^ " test #" ^ string_of_int idx ^ " raised an exception:\n");
          print_string (Printexc.to_string exn)
    end
    ts

let run_free_vars_tests () : unit =
  run_test "free_vars" free_vars free_vars_tests (list_to_string (fun x -> x))

let run_unused_vars_tests () : unit =
  run_test "unused_vars" unused_vars unused_vars_tests (list_to_string (fun x -> x))

let run_subst_tests () : unit =
  run_test "subst" (fun (s, e) -> subst s e) subst_tests Print.exp_to_string

let run_eval_tests () : unit =
  run_test "eval" eval eval_tests Print.exp_to_string

let run_infer_tests () : unit =
  run_test "infer" (fun (ctx, e) -> infer ctx e) infer_tests Print.typ_to_string

let run_unify_tests () : unit =
  run_test "unify" (fun (ty1, ty2) -> unify ty1 ty2) unify_tests (fun () -> "()")

let run_all_tests () : unit =
  run_free_vars_tests ();
  run_unused_vars_tests ();
  run_subst_tests ();
  run_eval_tests ();
  run_infer_tests ();
  run_unify_tests ()
