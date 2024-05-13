(* prim-type.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Primitive types are used in the SimpleAST and CFG IRs to characterize the runtime
 * representations of data.
 *)

structure PrimType : sig

    (* the representation kind of a type, which is either an unboxed tagged integer,
     * a boxed value (i.e., pointer), or has a mixed representation, where some values
     * are unboxed and some are boxed.
     *)
    datatype kind = Unboxed | Boxed | Mixed

    (* simplified types that track runtime representations *)
    datatype t
      = RAW                     (* raw (native) integer values *)
      | CODE                    (* code address *)
      | UNIFORM of kind         (* uniform values *)

    val kindOf : t -> kind

    (* MiniML types *)
    val intTy : t
    val refTy : t
    val stringTy : t
    val unitTy : t
    val anyTy : t
    val ptrTy : t

    (* return a string representation of a type *)
    val toString : t -> string

  end = struct

    datatype kind = Unboxed | Boxed | Mixed

    datatype t
      = RAW
      | CODE
      | UNIFORM of kind

    fun kindOf RAW = raise Fail "PrimType.kindOf: unexpected RAW"
      | kindOf CODE = Boxed
      | kindOf (UNIFORM k) = k

    (* MiniML types *)
    val intTy = UNIFORM Unboxed
    val refTy = UNIFORM Boxed
    val stringTy = UNIFORM Boxed
    val unitTy = UNIFORM Unboxed
    val anyTy = UNIFORM Mixed
    val ptrTy = UNIFORM Boxed

    fun toString RAW = "i64"
      | toString CODE = "code"
      | toString (UNIFORM Unboxed) = "i63"
      | toString (UNIFORM Boxed) = "ptr"
      | toString (UNIFORM Mixed) = "any"

  end
