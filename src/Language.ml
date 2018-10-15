(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap
open Ostap.Combinators

(* States *)
module State =
  struct
                                                                
    (* State: global state, local state, scope variables *)
    type t = {g : string -> int; l : string -> int; scope : string list}

    (* Empty state *)
    let empty =
      let e x = failwith (Printf.sprintf "Undefined variable: %s" x) in
      {g = e; l = e; scope = []}

    (* Update: non-destructively "modifies" the state s by binding the variable x 
       to value v and returns the new state w.r.t. a scope
    *)
    let update x v s =
      let u x v s = fun y -> if x = y then v else s y in
      if List.mem x s.scope then {s with l = u x v s.l} else {s with g = u x v s.g}

    (* Evals a variable in a state w.r.t. a scope *)
    let eval s x = (if List.mem x s.scope then s.l else s.g) x

    (* Creates a new scope, based on a given state *)
    let push_scope st xs = {empty with g = st.g; scope = xs}

    (* Drops a scope *)
    let drop_scope st st' = {st' with g = st.g}

  end
    
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
    
    let rec eval st expr =      
      match expr with
      | Const n -> n
      | Var   x -> State.eval st x
      | Binop (op, expr1, expr2) -> eval_bin_op op (eval st expr1) (eval st expr2)

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
                                                                                                                  
    *)
    ostap (                                      
      parse:
	  !(Ostap.Util.expr 
             (fun x -> x)
	     (Array.map (fun (a, s) -> a, 
                           List.map  (fun s -> ostap(- $(s)), (fun x y -> Binop (s, x, y))) s
                        ) 
              [|                
		`Lefta, ["!!"];
		`Lefta, ["&&"];
		`Nona , ["=="; "!="; "<="; "<"; ">="; ">"];
		`Lefta, ["+" ; "-"];
		`Lefta, ["*" ; "/"; "%"];
              |] 
	     )
	     primary);
      
      primary:
        n:DECIMAL {Const n}
      | x:IDENT   {Var x}
      | -"(" parse -")"
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
    (* loop with a post-condition       *) | Repeat of t * Expr.t
    (* call a procedure                 *) | Call   of string * Expr.t list with show
                                                                    
    (* The type of configuration: a state, an input stream, an output stream *)
    type config = State.t * int list * int list 

    let assign x e (s, i, o) = (State.update x (Expr.eval s e) s, i, o)

    let read x (s, (z :: i), o) = ((State.update x z s), i, o)

    let write e (s, i, o) = (s, i, o @ [Expr.eval s e])

    let rec eval_args args s = match args with
      | [] -> []
      | (arg :: args') -> (Expr.eval s arg) :: eval_args args' s

    let rec put_args values variables s = match (values, variables) with
      | ([], []) -> s
      | (v :: values', x :: variables') -> put_args values' variables' (State.update x v s)

    (* Statement evaluator

         val eval : env -> config -> t -> config

       Takes an environment, a configuration and a statement, and returns another configuration. The 
       environment supplies the following method

           method definition : string -> (string list, string list, t)

       which returns a list of formal parameters and a body for given definition
    *)
    let rec eval env c t = match t with
      | Assign (x, e) -> assign x e c
      | Read x -> read x c
      | Write e -> write e c      
      | Skip -> c
      | If (cond, th, el) -> let (s, _, _) = c in 
        if Expr.eval s cond <> 0 then eval env c th else eval env c el
      | While (cond, b) -> let (s, _, _) = c in 
        if Expr.eval s cond <> 0 then eval env (eval env c b) (While (cond, b)) else c
      | Repeat (b, cond)  -> (match (eval env c b) with
          | (s, i, o) -> if Expr.eval s cond <> 0 then (s, i, o) else eval env (s, i, o) (Repeat (b, cond)))
      | Call (f, l) -> let (args, locals, body) = env#definition f in
                       let (s, i, o) = c in 
                       let s' = put_args (eval_args l s) args (State.push_scope s (args @ locals)) in
                       let (s', i, o) = eval env (s', i, o) body in
                       (State.drop_scope s' s, i, o)
      | Seq (t, k) -> eval env (eval env c t) k
                                
    (* Statement parser *)
     ostap (
      simple_stmt:
        x:IDENT ":=" e:!(Expr.parse) {Assign (x, e)}
      | f:IDENT "(" args:(!(Util.list0) [Expr.parse]) ")" {Call (f, args)} (*args:(!(Util.list0By) [ostap (",")] [Expr.parse])*)
      | "read" "(" x:IDENT ")"         {Read x}
      | "write" "(" e:!(Expr.parse) ")" {Write e}
      | "skip" {Skip}
      | "while" cond:!(Expr.parse) "do" b:!(parse) "od" {While (cond, b)}
      | "repeat" b:!(parse) "until" cond:!(Expr.parse) {Repeat (b, cond)}
      | "for" pre_action:!(parse) -"," loop_cond:!(Expr.parse) -"," post_action:!(parse) "do" b:!(parse) "od" { Seq (pre_action, While (loop_cond, Seq (b, post_action))) }
      | if_statement;

      if_statement: "if" cond:!(Expr.parse) "then" th:!(parse) "fi" {If (cond, th, Skip)} 
                  | "if" cond:!(Expr.parse) "then" th:!(parse) el:!(else_statement) "fi" {If (cond, th, el)};
      else_statement: "else" b:!(parse) {b}             
                    | "elif" cond:!(Expr.parse) "then" th:!(parse) el:!(else_statement) {If (cond, th, el)}
                    | "elif" cond:!(Expr.parse) "then" th:!(parse) {If (cond, th, Skip)};             

      parse: <s::ss> : !(Util.listBy)[ostap (";")][simple_stmt] {List.fold_left (fun s ss -> Seq (s, ss)) s ss}
    )
      
  end

(* Function and procedure definitions *)
module Definition =
  struct

    (* The type for a definition: name, argument list, local variables, body *)
    type t = string * (string list * string list * Stmt.t)

    ostap (                                      
      parse: "fun" name:IDENT "(" args:!(variables) ")" locals:!(loc) "{" body:!(Stmt.parse) "}" {(name, (args, locals, body))};
      variables: x:IDENT "," r:!(variables) {[x] @ r} | x:IDENT {[x]} | empty {[]};
      loc:"local" x:IDENT "," r:!(variables) {[x] @ r} | "local" x:IDENT {[x]} | "local" {[]} | empty {[]}
    )

  end
    
(* The top-level definitions *)

(* The top-level syntax category is a pair of definition list and statement (program body) *)
type t = Definition.t list * Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval (defs, body) i =
  let module M = Map.Make (String) in
  let m        = List.fold_left (fun m ((name, _) as def) -> M.add name def m) M.empty defs in  
  let _, _, o  = Stmt.eval (object method definition f = snd @@ M.find f m end) (State.empty, i, []) body in o

(* Top-level parser *)
let parse = ostap (!(Definition.parse)* !(Stmt.parse))
