(* cfg.sml
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
 *)

signature CFG =
  sig

    type var
    type label

    datatype value
      = VAR of var                              (* locally bound variable *)
      | INT of IntInf.int                       (* tagged integer literal *)
      | STRING of string                        (* string literal *)
      | CODE of label                           (* function label *)

    (* a MiniML function application.  The type is the return type of the function,
     * which we need when generating LLVM code.
     *)
    type application = PrimType.t * value * value list

    (* a labeled expression *)
    datatype exp = EXP of ProgPt.t * exp_rep

    and exp_rep
      = LET of var * rhs * exp          (* let binding *)
      | IF of PrimCond.t * value list * jump * jump (* conditional *)
      | TAIL_APPLY of application       (* tail application of function *)
      | GOTO of jump                    (* jump to a fragment in the same function *)
      | RETURN of value                 (* return from a function call *)

    and rhs
      = APPLY of application            (* non-tail application of a function *)
      | CALL of Runtime.t * value list  (* call to runtime-system function *)
      | PRIM of Prim.t * value list     (* primitive operator *)
      | ALLOC of value list             (* tuple allocation *)
      | SEL of int * value              (* tuple select *)

    and frag = FRAG of {                (* code fragments (these are essentially
                                         * extended basic blocks).
                                         *)
          lab : label,                  (* label *)
          params : var list,            (* live variables incoming to the fragment *)
          body : exp                    (* the fragment body (an extended basic block) *)
        }

    and function = FUN of {             (* a CFG function *)
          retTy : PrimType.t,           (* return type of the function *)
          entry : frag,                 (* the entry fragment *)
          frags : frag list             (* the other fragments in the function *)
        }

    withtype jump = label * value list  (* jump to fragment *)

    (* a program is a list of functions, where the first function is
     * the entry for the whole program.
     *)
    datatype program = PROG of function list

    (* smart constructor for expressions *)
    val mkLET        : var * rhs * exp -> exp
    val mkIF         : PrimCond.t * value list  * jump * jump -> exp
    val mkTAIL_APPLY : application -> exp
    val mkGOTO       : jump -> exp
    val mkRETURN     : value -> exp

  end

structure CFG :> CFG
    where type var = CFGRep.var
    where type label = CFGRep.label
    where type exp = CFGRep.exp
    where type rhs = CFGRep.rhs
    where type frag = CFGRep.frag
    where type function = CFGRep.function
  = struct

    open CFGRep

    type application = PrimType.t * value * value list

    local
      fun mkEXP mk arg = EXP(ProgPt.new(), mk arg)
    in
    val mkLET = mkEXP LET
    val mkIF = mkEXP IF
    val mkTAIL_APPLY = mkEXP TAIL_APPLY
    val mkGOTO = mkEXP GOTO
    val mkRETURN = mkEXP RETURN
    end (* local *)

  end
