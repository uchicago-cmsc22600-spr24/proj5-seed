(* parse-tree.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Parse-tree representation of MiniML programs
 *)

structure ParseTree =
  struct

    type 'a mark = 'a Error.mark                (* {span : span, tree : 'a} *)

    type id = Atom.atom

    datatype program
      = MarkProg of program mark                (* source-file mark *)
      | Prog of top_dcl list                    (* MiniML program *)

    and top_dcl
      = MarkDcl of top_dcl mark                 (* source-file mark *)
      | TypeDcl of id * id list * con list      (* data type declaration *)
      | ValDcl of bind                          (* value declaration *)

    and ty
      = MarkTy of ty mark                       (* source-file mark *)
      | FunTy of ty * ty                        (* function type *)
      | TupleTy of ty list                      (* tuple type; the list must have more
                                                 * than one element
                                                 *)
      | ConTy of id * ty list                   (* type constructor application; the
                                                 * argument list may be empty
                                                 *)
      | VarTy of id                             (* type variable *)

    and con
      = MarkCon of con mark                     (* source-file mark *)
      | Con of id * ty list                     (* data constructor definition *)

    and bind
      = MarkBind of bind mark                   (* source-file mark *)
      | FunBind of id * pat list * exp          (* function declaration; the parameters
                                                 * must be tuple, variable, or
                                                 * wildcard patterns
                                                 *)
      | ValBind of pat * exp                    (* value identifier declaration; the
                                                 * lhs must be a tuple, variable, or
                                                 * wildcard pattern
                                                 *)
      | DoExpBind of exp                        (* `do` expression binding *)

    and exp
      = MarkExp of exp mark                     (* source-file mark *)
      | IfExp of exp * exp * exp                (* conditional *)
      | OrElseExp of exp * exp                  (* `||` conditional operator *)
      | AndAlsoExp of exp * exp                 (* `&&` conditional operator *)
      | BinExp of exp * id * exp                (* infix binary operator *)
      | UnExp of id * exp                       (* unary operator *)
      | AppExp of exp * exp                     (* value application *)
      | TupleExp of exp list                    (* tuple expression; the list must
                                                 * have either zero or more than one
                                                 * items.
                                                 *)
      | VarExp of id                            (* value identifier *)
      | ConExp of id                            (* data constructor identifier *)
      | IntExp of IntInf.int                    (* integer literal *)
      | StrExp of string                        (* string literal *)
      | CaseExp of exp * rule list              (* case expression *)
      | BindExp of bind * exp                   (* desugared scope expression *)

    and rule
      = MarkRule of rule mark                   (* source-file mark *)
      | CaseRule of pat * exp                   (* pattern matching rule in a case
                                                 * expression
                                                 *)

    and pat
      = MarkPat of pat mark                     (* source-file mark *)
      | TuplePat of pat list                    (* tuple pattern; the list must have
                                                 * either zero or more than one items.
                                                 *)
      | ConPat of id * pat list                 (* data constructor pattern *)
      | VarPat of id                            (* variable binding pattern *)
      | WildPat                                 (* wild-card pattern *)

  end
