(* cond.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Primitive conditional operators for MiniML.  Note that these can only appear
 * in conditional expressions.
 *)

structure PrimCond =
  struct

    (* primitive conditional operators; note that the `Boxed` test is CFG only *)
    datatype t
      = IntLt           (* signed integer `<` test *)
      | IntLte          (* signed integer `<=` test *)
      | IntEq           (* signed integer equality test *)
      | IntNEq          (* signed integer inequality test *)
      | UIntLt          (* unsigned integer `<` test; used for bounds checking *)
      | Boxed           (* boxity test (i.e., test if the argument a pointer) *)

    (* return the number of arguments of the operator *)
    fun arityOf Boxed = 1
      | arityOf _ = 2

    (* are two primops the same? *)
    fun same (p1 : t, p2) = (p1 = p2)

    (* convert to string *)
    fun toString IntLt = "IntLt"
      | toString IntLte = "IntLte"
      | toString IntEq = "IntEq"
      | toString IntNEq = "IntNEq"
      | toString UIntLt = "UIntLt"
      | toString Boxed = "Boxed"

  end
