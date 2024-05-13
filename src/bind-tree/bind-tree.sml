(* bind-tree.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Parse-tree representation of MiniML programs with binding information.
 *)

structure BindTree =
  struct

    (* a term marked with a source location *)
    type 'a mark = 'a Error.mark (* {span : span, tree : 'a} *)

    (* define the various classes of identifiers *)
    structure TyVar = IdentifierFn()    (* type variable identifiers *)
    structure TycId = IdentifierFn()    (* type constructor identifiers *)
    structure ConId = IdentifierFn()    (* data constructor identifiers *)
    structure ValId = IdentifierFn()    (* value-variable identifiers *)

    (* type aliases *)
    type tyvar = TyVar.t
    type tycon = TycId.t
    type conid = ConId.t
    type valid = ValId.t

    type number = IntInf.int                    (* integer literal *)

    datatype program
      = MarkProg of program mark                (* source-file mark *)
      | Prog of declaration list                (* program *)

    and declaration
      = MarkDcl of declaration mark             (* source-file mark *)
      | TypeDcl of tycon * tyvar list * con list
                                                (* type declaration *)
      | ValDcl of bind                          (* value declaration *)

    and ty
      = MarkTy of ty mark                       (* source-file mark *)
      | FunTy of ty * ty                        (* function type *)
      | TupleTy of ty list                      (* tuple type; list has 2+ elements *)
      | ConTy of tycon * ty list                (* type constructor application; the
                                                 * argument list may be empty
                                                 *)
      | VarTy of tyvar                          (* type variable *)

    and con
      = MarkCon of con mark                     (* source-file mark *)
      | Con of conid * ty list                  (* data constructor declaration *)

    and bind
      = MarkBind of bind mark                   (* source-file mark *)
      | FunBind of valid * pat list * exp       (* function declaration; the parameters
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
      | BinExp of exp * valid * exp             (* infix binary operators *)
      | UnExp of valid * exp                    (* unary operator *)
      | AppExp of exp * exp                     (* value application *)
      | TupleExp of exp list                    (* tuple; the list should have zero
                                                 * or more than one items.
                                                 *)
      | VarExp of valid                         (* variable-use occurrence *)
      | ConExp of conid                         (* data constructor identifier *)
      | IntExp of number                        (* integer literal *)
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
      | TuplePat of pat list                    (* tuple pattern; the list should have
                                                 * zero or more than one items.
                                                 *)
      | ConPat of conid * pat list              (* data constructor pattern *)
      | VarPat of valid                         (* variable binding pattern *)
      | WildPat                                 (* wild-card pattern *)

  end
