(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap
open Ostap.Combinators
       
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)
                                                            
    (* State: a partial map from variables to integer values. *)
    type state = string -> int 

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x 
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)                                                       
    let int_to_bool n = if n == 0 then false else true
    
    let bool_to_int b = if b then 1 else 0

    let eval_bin_op op left right = match op with
      | "+" -> left + right
      | "-" -> left - right
      | "*" -> left * right
      | "/" -> left / right
      | "%" -> left mod right
      | "<" -> bool_to_int (left < right)
      | "<=" -> bool_to_int (left <= right)
      | ">" -> bool_to_int (left > right)
      | ">=" -> bool_to_int (left >= right)
      | "==" -> bool_to_int (left == right)
      | "!=" -> bool_to_int (left != right)
      | "&&" -> bool_to_int ((int_to_bool left) && (int_to_bool right))
      | "!!" -> bool_to_int ((int_to_bool left) || (int_to_bool right))
      | _ -> failwith "undefined binary operator"

    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)
    let rec eval state expr = match expr with
      | Const n -> n
      | Var x -> state x
      | Binop (op, expr1, expr2) -> eval_bin_op op (eval state expr1) (eval state expr2)

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
   
    *)
    ostap (
      expr:
        !(Util.expr 
               (fun x -> x)
               [|
                 `Lefta , [ostap ("!!" ), (fun x y -> Binop ("!!", x, y))];
                 `Lefta , [ostap ("&&" ), (fun x y -> Binop ("&&", x, y))];
                 `Nona  , [ostap ("<="), (fun x y -> Binop ("<=", x, y)); ostap ("<"), (fun x y -> Binop ("<", x, y)); 
                           ostap (">="), (fun x y -> Binop (">=", x, y)); ostap (">"), (fun x y -> Binop (">", x, y)); 
                           ostap ("=="), (fun x y -> Binop ("==", x, y)); ostap ("!="), (fun x y -> Binop ("!=", x, y))];
                 `Lefta , [ostap ("+" ), (fun x y -> Binop ("+", x, y)); ostap ("-"), (fun x y -> Binop ("-", x, y))];
                 `Lefta , [ostap ("*" ), (fun x y -> Binop ("*", x, y)); ostap ("/"), (fun x y -> Binop ("/", x, y)); 
                           ostap ("%"), (fun x y -> Binop ("%", x, y))]
               |]
               primary
        );
      primary: x:IDENT {Var x} | n:DECIMAL {Const n} | -"(" expr -")"                     
    )

  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t 
    (* empty statement                  *) | Skip
    (* conditional                      *) | If     of Expr.t * t * t
    (* loop with a pre-condition        *) | While  of Expr.t * t
    (* loop with a post-condition       *) | Until  of t * Expr.t  with show
                                                                    
    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

         val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let assign x e (s, i, o) = (Expr.update x (Expr.eval s e) s, i, o)

    let read x (s, (z :: i), o) = ((Expr.update x z s), i, o)

    let write e (s, i, o) = (s, i, o @ [Expr.eval s e])

    (* Statement evaluator

          val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval c t = match t with
      | Assign (x, e) -> assign x e c
      | Read x -> read x c
      | Write e -> write e c      
      | Skip -> c
      | If (cond, th, el) -> let (s, _, _) = c in 
        if Expr.eval s cond <> 0 then eval c th else eval c el
      | While (cond, b) -> let (s, _, _) = c in 
        if Expr.eval s cond <> 0 then eval (eval c b) (While (cond, b)) else c
      | Until (b, cond)  -> (match (eval c b) with
          | (s, i, o) -> if Expr.eval s cond <> 0 then (s, i, o) else eval (s, i, o) (Until (b, cond)))
      | Seq (t, k) -> eval (eval c t) k
    (* Statement parser *)
    ostap (
      simple_stmt:
        x:IDENT ":=" e:!(Expr.expr) {Assign (x, e)}
      | "read" "(" x:IDENT ")"         {Read x}
      | "write" "(" e:!(Expr.expr) ")" {Write e}
      | "skip" {Skip}
      | "while" cond:!(Expr.expr) "do" b:!(parse) "od" {While (cond, b)}
      | "repeat" b:!(parse) "until" cond:!(Expr.expr) {Until (b, cond)}
      | "for" pre_action:!(parse) -"," loop_cond:!(Expr.expr) -"," post_action:!(parse) "do" b:!(parse) "od" { Seq (pre_action, While (loop_cond, Seq (b, post_action))) }
      | if_statement;

      if_statement: "if" cond:!(Expr.expr) "then" th:!(parse) "fi" {If (cond, th, Skip)} 
                  | "if" cond:!(Expr.expr) "then" th:!(parse) el:!(else_statement) "fi" {If (cond, th, el)};
      else_statement: "else" b:!(parse) {b}             
                    | "elif" cond:!(Expr.expr) "then" th:!(parse) el:!(else_statement) {If (cond, th, el)}
                    | "elif" cond:!(Expr.expr) "then" th:!(parse) {If (cond, th, Skip)};

      parse: <s::ss> : !(Util.listBy)[ostap (";")][simple_stmt] {List.fold_left (fun s ss -> Seq (s, ss)) s ss}
    )
      
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval p i =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o

(* Top-level parser *)
let parse = Stmt.parse                                                     
