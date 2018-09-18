open GT       
       
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
type config = int list * Syntax.Stmt.config

(* Utility functions used for 'eval' function *)

let eval_bin_op op (y :: x :: st, c) = ((Syntax.Expr.eval_bin_op op x y) :: st, c)

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
    | ST x -> eval (let z :: st' = st in (st', (Syntax.Expr.update x z s, i, o))) p'

(* Top-level evaluation

     val run : int list -> prg -> int list

   Takes an input stream, a program, and returns an output stream this program calculates
*)
let run i p = let (_, (_, _, o)) = eval ([], (Syntax.Expr.empty, i, [])) p in o

(* utility functions for 'compile' function *)

let rec compileExpr expr = match expr with
  | Syntax.Expr.Binop (op, x, y) -> compileExpr x @ compileExpr y @ [BINOP op]
  | Syntax.Expr.Const n -> [CONST n]
  | Syntax.Expr.Var x -> [LD x]

(* Stack machine compiler

     val compile : Syntax.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)

let rec compile t = match t with
  | Syntax.Stmt.Assign (x, e) -> compileExpr e @ [ST x]
  | Syntax.Stmt.Read x -> [READ; ST x]
  | Syntax.Stmt.Write e -> compileExpr e @ [WRITE]
  | Syntax.Stmt.Seq (s1, s2) -> compile s1 @ compile s2
