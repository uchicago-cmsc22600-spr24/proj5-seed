(* ast.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Typed Abstract-Syntax Tree representation for MiniML programs
 *)

structure AST =
  struct

    type valid = Var.t
    type conid = DataCon.t

    type ty = Type.ty

    datatype program
      = Prog of top_dcl list                    (* MiniML program *)

    and top_dcl
      = TypeDcl of Type.usr_tyc                 (* user type declaration *)
      | ValDcl of bind                          (* value declaration; bind is not ExpBind *)

    and bind
      = FunBind of valid * pat list * exp       (* function definition *)
      | ValBind of pat * exp                    (* value identifier definition *)

    and exp = E of exp_rep * ty                 (* expression paired with its type *)

    (* abstract syntax of expressions *)
    and exp_rep
      = IfExp of exp * exp * exp                (* conditional *)
      | AppExp of exp * exp                     (* value application *)
      | TupleExp of exp list                    (* tuple expression; list has 0 or
                                                 * 2+ elements
                                                 *)
      | CaseExp of exp * rule list              (* case expression *)
      | BindExp of bind * exp                   (* local scope *)
      | VarExp of valid                         (* value variable *)
      | ConExp of conid                         (* data constructor identifier *)
      | IntExp of IntInf.int                    (* integer literal *)
      | StrExp of string                        (* string literal *)

    and rule
      = CaseRule of pat * exp                   (* pattern matching rule in a case
                                                 * expression
                                                 *)

    and pat
      = ConPat of conid * pat list              (* data constructor pattern *)
      | TuplePat of pat list                    (* tuple pattern; list has 0 or
                                                 * 2+ elements
                                                 *)
      | VarPat of valid                         (* variable binding pattern *)

  end
