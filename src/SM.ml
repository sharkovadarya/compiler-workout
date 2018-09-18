open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Utility functions used for 'eval' function *)

let eval_bin_op op (y :: x :: st, c) = ((Language.Expr.eval_bin_op op x y) :: st, c)

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)                         
let rec eval (st, (s, i, o)) p = match p with 
  | [] -> (st, (s, i, o))
  | inst :: p' -> match inst with
    | BINOP op -> eval (eval_bin_op op (st, (s, i, o))) p'
    | CONST n ->  eval (n :: st, (s, i, o)) p'
    | READ -> eval (let z :: i' = i in (z :: st, (s, i', o))) p'
    | WRITE -> eval (let z :: st' = st in (st, (s, i, o @ [z]))) p'
    | LD x -> eval ((s x) :: st, (s, i, o)) p'
    | ST x -> eval (let z :: st' = st in (st', (Language.Expr.update x z s, i, o))) p'


(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i = let (_, (_, _, o)) = eval ([], (Language.Expr.empty, i, [])) p in o

(* utility functions for 'compile' function *)

let rec compileExpr expr = match expr with
  | Language.Expr.Binop (op, x, y) -> compileExpr x @ compileExpr y @ [BINOP op]
  | Language.Expr.Const n -> [CONST n]
  | Language.Expr.Var x -> [LD x]

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)

let rec compile t = match t with
  | Language.Stmt.Assign (x, e) -> compileExpr e @ [ST x]
  | Language.Stmt.Read x -> [READ; ST x]
  | Language.Stmt.Write e -> compileExpr e @ [WRITE]
  | Language.Stmt.Seq (s1, s2) -> compile s1 @ compile s2