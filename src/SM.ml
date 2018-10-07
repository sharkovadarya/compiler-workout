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
(* conditional jump                *) | CJMP  of string * string with show
                                                   
(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Utility functions used for 'eval' function *)

let eval_bin_op op (y :: x :: st, c) = ((Language.Expr.eval_bin_op op x y) :: st, c)

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)                            
let rec eval e (st, (s, i, o)) p = match p with 
  | [] -> (st, (s, i, o))
  | inst :: p' -> match inst with
    | BINOP op -> eval e (eval_bin_op op (st, (s, i, o))) p'
    | CONST n ->  eval e (n :: st, (s, i, o)) p'
    | READ -> eval e (let z :: i' = i in (z :: st, (s, i', o))) p'
    | WRITE -> eval e (let z :: st' = st in (st, (s, i, o @ [z]))) p'
    | LD x -> eval e ((s x) :: st, (s, i, o)) p'
    | ST x -> eval e (let z :: st' = st in (st', (Language.Expr.update x z s, i, o))) p'
    | LABEL _ -> eval e (st, (s, i, o)) p'
    | JMP l -> eval e (st, (s, i, o)) (e#labeled l)
    | CJMP (c, l) -> let z :: st' = st in 
      (if ((c = "z") && (z = 0) || (c = "nz") && (z <> 0)) 
       then (eval e (st', (s, i, o)) (e#labeled l)) 
       else (eval e (st, (s, i, o)) p'))

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i =
  let module M = Map.Make (String) in
  let rec make_map m = function
  | []              -> m
  | (LABEL l) :: tl -> make_map (M.add l tl m) tl
  | _ :: tl         -> make_map m tl
  in
  let m = make_map M.empty p in
  let (_, (_, _, o)) = eval (object method labeled l = M.find l m end) ([], (Expr.empty, i, [])) p in o


let make_label i = Printf.sprintf "L%d" i
(* Stack machine compiler

     val compile : Language.Stmt.t -> prg
 
   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
let rec compile' i =
  let rec expr = function
  | Expr.Var   x          -> [LD x]
  | Expr.Const n          -> [CONST n]
  | Expr.Binop (op, x, y) -> expr x @ expr y @ [BINOP op]
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

  | Stmt.Until (b, cond) -> let body, j = compile' (i + 1) b in
                           [LABEL (make_label i)] @ body @ (expr cond) @ [CJMP ("z", (make_label i))], j

let compile t = let res, _ = compile' 0 t in res