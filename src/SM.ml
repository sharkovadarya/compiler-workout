open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string
(* a label                         *) | LABEL of string
(* unconditional jump              *) | JMP   of string                                                                                                                
(* conditional jump                *) | CJMP  of string * string
(* begins procedure definition     *) | BEGIN of string list * string list
(* end procedure definition        *) | END
(* calls a procedure               *) | CALL  of string with show
                                                   
(* The type for the stack machine program *)                                                               
type prg = insn list
                            
(* The type for the stack machine configuration: control stack, stack and configuration from statement
   interpreter
 *)
type config = (prg * State.t) list * int list * Expr.config


let eval_bin_op op (w, y :: x :: st, c) = (w, (Language.Expr.eval_bin_op op x y) :: st, c)

let load x (w, st, (s, i, o)) = (w, (Language.State.eval s x) :: st, (s, i, o))

let store x (w, z :: st, (s, i, o)) = (w, st, (Language.State.update x z s, i, o))

let rec put_args args c = match args with
  | [] -> c
  | (arg :: args') -> put_args args' (store arg c)


(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)

let rec eval env ((w, st, (s, i, o)) as c) p = match p with
  | [] -> (w, st, (s, i, o))
  | inst :: p' -> match inst with
    | BINOP op -> eval env (eval_bin_op op (w, st, (s, i, o))) p'
    | CONST n -> eval env (w, n :: st, (s, i, o)) p'
    | READ -> eval env (let z :: i' = i in (w, z :: st, (s, i', o))) p'
    | WRITE -> eval env (let z :: st' = st in (w, st, (s, i, o @ [z]))) p'
    | LD x -> eval env (load x (w, st, (s, i, o))) p'
    | ST x  -> eval env (store x (w, st, (s, i, o))) p'
    | LABEL _ -> eval env (w, st, (s, i, o)) p'
    | JMP l -> eval env (w, st, (s, i, o)) (env#labeled l)
    | BEGIN (args, locals) -> let c = (w, st, (s, i, o)) in 
                              let (w, st, (c, i, o)) = c in
                              let c = State.enter c (args @ locals) in
                                eval env (put_args args (w, st, (c, i, o))) p'
    | END -> (match w with
      | [] -> (w, st, (s, i, o))
      | (p, s') :: w' -> eval env (w', st, (State.leave s s', i, o)) p)
    | CALL f -> let c = (w, st, (s, i, o)) in 
                let (w, st, (c, i, o)) = c in
                  eval env ((p', c) :: w, st, (c, i, o)) (env#labeled f)
    | CJMP (m, l) -> match (w, st, (s, i, o)) with
      | (w, z :: st, c) -> if ((m = "z") && (z = 0)) || ((m = "nz") && (z <> 0)) then (eval env (w, st, c) (env#labeled l)) else (eval env (w, st, c) p')

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i =
  (*print_prg p;*)
  let module M = Map.Make (String) in
  let rec make_map m = function
  | []              -> m
  | (LABEL l) :: tl -> make_map (M.add l tl m) tl
  | _ :: tl         -> make_map m tl
  in
  let m = make_map M.empty p in
  let (_, _, (_, _, o)) = eval (object method labeled l = M.find l m end) ([], [], (State.empty, i, [])) p in o

let make_label i = Printf.sprintf "L%d" i

let rec compile' i =
  let rec expr = function
  | Expr.Var   x          -> [LD x]
  | Expr.Const n          -> [CONST n]
  | Expr.Binop (op, x, y) -> expr x @ expr y @ [BINOP op]
  | Expr.Call (f, l)      -> let res, m = compile' i (Stmt.Call(f, l)) in
                             res
  in
  let rec expr_fold inss = match inss with
    | []   -> []
    | (ins :: inss') -> (expr_fold inss') @ (expr ins)
  in
  function
  | Stmt.Seq (s1, s2)  -> let c1, j = compile' i s1 in
                          let c2, k = compile' j s2 in
                          c1 @ c2, k
  | Stmt.Read x        -> [READ; ST x], i
  | Stmt.Write e       -> expr e @ [WRITE], i
  | Stmt.Assign (x, e) -> expr e @ [ST x], i
  | Stmt.Skip -> [], i
  | Stmt.If (cond, th, el) -> let cond' = expr cond in
                              let then_body, j = compile' (i + 1) th in
                              let else_body, k = compile' (j + 1) el in
                              cond' @ [CJMP ("z", (make_label i))] @ 
                              then_body @ [JMP (make_label j)] @ [LABEL (make_label i)] @ 
                              else_body @ [JMP (make_label j)] @ [LABEL (make_label j)], k
  | Stmt.While (cond, b) -> let cond' = expr cond in
                            let body, j = compile' (i + 2) b in 
                            [LABEL (make_label i)] @ cond' @ [CJMP ("z", (make_label (i + 1)))] @ 
                            body @ [JMP (make_label i)] @ [LABEL (make_label (i + 1))], j

  | Stmt.Repeat (b, cond) -> let body, j = compile' (i + 1) b in
                           [LABEL (make_label i)] @ body @ (expr cond) @ [CJMP ("z", (make_label i))], j
  | Stmt.Call (f, l)     -> expr_fold l @ [CALL f], i
  | Stmt.Return None     -> [END], i
  | Stmt.Return (Some e) -> (expr e) @ [END], i

let rec compile_functions i = function
    | [] -> [], i
    | (name, (args, locals, body)) :: l -> let compiled_body, i = compile' i body in
                                           let r, i = compile_functions i l in
                                             [LABEL name] @ [BEGIN (args, locals)] @ compiled_body @ [END] @ r, i

(* Stack machine compiler

     val compile : Language.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
let compile (functions, p) = let definitions, i = compile_functions 0 functions in
                             let p', _ = compile' i p in
                               p' @  [END] @ definitions  