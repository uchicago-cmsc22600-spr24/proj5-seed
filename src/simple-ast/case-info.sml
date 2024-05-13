(* case-info.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Information about the domain of SimpleAST case expressions. which is
 * recorded at their program points.  This information is recorded during
 * simplification and used during case splitting.
 *)

structure CaseInfo : sig

    (* information about the data constructors in the domain of a case *)
    type t = {
        ty : PrimType.t,                (* the result type of the case *)
        argKind : PrimType.kind,        (* the kind of the argument type *)
        nConsts : int,                  (* the number of nullary constructors in the
                                         * argument type
                                         *)
        nFuns : int                     (* the number of constructor functions in the
                                         * argument type
                                         *)
      }

    (* get/set information for a case expression.  These functions raise `Fail` if the
     * expression is not a `CaseExp`.
     *)
    val set : SimpleAST.exp * t -> unit
    val get : SimpleAST.exp -> t
    val getKind : SimpleAST.exp -> PrimType.kind

  end = struct

    type t = { ty : PrimType.t, argKind : PrimType.kind, nConsts : int, nFuns : int }

    val {getFn : ProgPt.t -> t, setFn, ...} =
          ProgPt.newProp (fn _ => raise Fail "no case info")

    fun set (SimpleAST.E(ppt, SimpleAST.E_CASE _), info) = setFn(ppt, info)
      | set _ = raise Fail "CaseInfo.set: not a case expression"

    fun get (SimpleAST.E(ppt, SimpleAST.E_CASE _)) = getFn ppt
      | get _ = raise Fail "CaseInfo.set: not a case expression"

    fun getKind e = #argKind(get e)

  end
