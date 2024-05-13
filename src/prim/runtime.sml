(* runtime.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Runtime-system functions for MiniML.
 *)

structure Runtime : sig

    type t

    (* return the function's name *)
    val nameOf : t -> string

    (* return the function's parameter and result type.  If the result type is `void`
     * then the result type will be `NONE`.
     *)
    val typeOf : t -> PrimType.t list * PrimType.t option

    (* return the arity of the function *)
    val arityOf : t -> int

    (* return true if the function is pure; i.e., has no visible side effects *)
    val isPure : t -> bool

    (* are two runtime functions the same? *)
    val same : t * t -> bool

    (* runtime functions *)
    val funConcat : t           (* string concatenation *)
    val funExit : t             (* terminate the program with a status code *)
    val funFail : t             (* terminate the program with an error message *)
    val funPrint : t            (* print a string to the standard output *)
    val funStrChr : t           (* convert an ASCII code point to a string *)

  end = struct

    structure Ty = PrimType

    datatype ty = datatype Ty.t

    datatype t = CF of {
        name : string,                  (* the function's name *)
        pure : bool,                    (* true for pure functions *)
        paramTys : ty list,             (* parameter types *)
        resTy : ty option               (* result type; `NONE` means `void` *)
      }

    fun nameOf (CF{name, ...}) = name

    fun typeOf (CF{paramTys, resTy, ...}) = (paramTys, resTy)

    fun arityOf (CF{paramTys, resTy, ...}) = List.length paramTys

    fun isPure (CF{pure, ...}) = pure

    (* initialize a runtime function representation *)
    fun new (name, pure, tys, resTy) = CF{
            name = "_mml_" ^ name,
            pure = pure,
            paramTys = tys,
            resTy  = resTy
          }

    fun same (CF{name=a, ...}, CF{name=b, ...}) = (a = b)

    val funConcat = new ("concat", true, [Ty.stringTy, Ty.stringTy], SOME Ty.stringTy)
    val funExit = new ("exit", false, [Ty.intTy], NONE)
    val funFail = new ("fail", false, [Ty.stringTy], NONE)
    val funPrint = new ("print", false, [Ty.stringTy], NONE)
    val funStrChr = new ("str_chr", true, [Ty.intTy], SOME Ty.stringTy)

  end
