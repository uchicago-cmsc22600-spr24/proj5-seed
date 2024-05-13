(* prim.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Primitive operators for MiniML.  These do *not* include the conditional operators,
 * which are defined in the `PrimCond` module.
 *)

structure Prim =
  struct

    (* primitive operators; note that the Boxed test is CFG only *)
    datatype t
      = IntAdd | IntSub | IntMul | IntDiv | IntMod | IntNeg
      | StrSize | StrSub
      | RefAssign | RefDeref | RefNew

    (* return the number of arguments of the operator *)
    fun arityOf IntNeg = 1
      | arityOf StrSize = 1
      | arityOf RefDeref = 1
      | arityOf RefNew = 1
      | arityOf _ = 2

    (* return the result type of the primitive operator *)
    fun resultTypeOf RefDeref = PrimType.anyTy
      | resultTypeOf RefNew = PrimType.refTy
      | resultTypeOf _ = PrimType.intTy

    (* return true if the primop is pure; i.e., has no visible side effects *)
    fun isPure RefAssign = false
      | isPure IntDiv = false
      | isPure IntMod = false
      | isPure StrSub = false
      | isPure _ = true

    (* are two primops the same? *)
    fun same (p1 : t, p2) = (p1 = p2)

    (* convert to string *)
    fun toString IntAdd = "IntAdd"
      | toString IntSub = "IntSub"
      | toString IntMul = "IntMul"
      | toString IntDiv = "IntDiv"
      | toString IntMod = "IntMod"
      | toString IntNeg = "IntNeg"
      | toString StrSize = "StrSize"
      | toString StrSub = "StrSub"
      | toString RefAssign = "RefAssign"
      | toString RefDeref = "RefDeref"
      | toString RefNew = "RefNew"

  end
