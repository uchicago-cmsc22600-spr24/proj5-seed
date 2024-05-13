(* cfg-rep.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * This is a first-order control-flow graph representation of MiniML programs.
 * A program is represented as a list of first-order functions.  The first function
 * in the list corresponds to the top-level expression of the SimpleAST.
 * See the `cfg.sml` file for the documentation of the `exp`, `exp_rep`, etc.
 * datatypes.
 *
 * The public interface to tht CFG representation is defined in the
 * CFG, CFGVar, and CFGLabel structures.
 *)

structure CFGRep =
  struct

    datatype exp = EXP of ProgPt.t * exp_rep

    and exp_rep
      = LET of var * rhs * exp
      | IF of PrimCond.t * value list * jump * jump
      | TAIL_APPLY of PrimType.t * value * value list
      | GOTO of jump
      | RETURN of value

    and rhs
      = APPLY of PrimType.t * value * value list
      | CALL of Runtime.t * value list
      | PRIM of Prim.t * value list
      | ALLOC of value list
      | SEL of int * value

    and value
      = VAR of var
      | INT of IntInf.int
      | STRING of string
      | CODE of label

    and frag
      = FRAG of {
          lab : label,
          params : var list,
          body : exp
        }

    and function
      = FUN of {
          retTy : PrimType.t,
          entry : frag,
          frags : frag list
        }

    and var = V of {                    (* CFG variables *)
        name : string,                  (* variable name *)
        id : Stamp.t,                   (* unique ID for the variable *)
        ty : PrimType.t,                (* the variable's type *)
        props : PropList.holder         (* for algorithm-specific info *)
      }

    and label = L of {                  (* CFG labels *)
        name : string,                  (* label name *)
        id : Stamp.t,                   (* unique ID for the label *)
        kind : label_kind ref,          (* label binding info *)
        export : bool,                  (* true for entry labels that are exporte *)
        props : PropList.holder         (* for algorithm-specific info *)
      }

    and label_kind
      = LK_None                         (* placeholder *)
      | LK_Export of string             (* the program's entry function's label *)
      | LK_Entry of function            (* function entry *)
      | LK_Local of frag                (* local fragment label *)
      | LK_Extern                       (* external runtime-system function *)

    withtype jump = label * value list  (* jump to fragment *)

    datatype program = PROG of function list

  end
