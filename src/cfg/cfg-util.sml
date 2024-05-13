(* cfg-util.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Utility functions for the CFG IR (mostly for debugging)
 *)

structure CFGUtil : sig

  (* return the type of a value *)
    val typeOfValue : CFG.value -> PrimType.t

  (* printable versions of some CFG terms *)
    val valueToString : CFG.value -> string
    val rhsToString : CFG.rhs -> string

  end = struct

    fun typeOfValue (CFG.VAR x) = CFGVar.typeOf x
      | typeOfValue (CFG.INT _) = PrimType.intTy
      | typeOfValue (CFG.STRING _) = PrimType.stringTy
      | typeOfValue (CFG.CODE _) = PrimType.ptrTy

    fun valueToString (CFG.VAR x) = CFGVar.toString x
      | valueToString (CFG.INT n) = if (n < 0)
          then "-" ^ IntInf.toString(~n)
          else IntInf.toString n
      | valueToString (CFG.STRING s) = concat["\"", String.toString s, "\""]
      | valueToString (CFG.CODE lab) = CFGLabel.toString lab

    val valuesToString = String.concatWithMap "," valueToString

    fun rhsToString rhs = (case rhs
           of CFG.APPLY(_, f, vs) => concat[
                  "apply ", valueToString f, " (", valuesToString vs, ")"
                ]
            | CFG.CALL(cf, vs) => concat[
                  "call ", Runtime.nameOf cf, " (", valuesToString vs, ")"
                ]
            | CFG.PRIM(p, vs) => concat[Prim.toString p, " (", valuesToString vs, ")"]
            | CFG.ALLOC vs => concat["alloc ", "[", valuesToString vs, "]"]
            | CFG.SEL(i, v) => concat["#", Int.toString i, "(", valueToString v, ")"]
          (* end case *))

  end
