(* llvm-type.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *)

structure LLVMType : sig

    datatype t
      = Void
      | Int1                    (* a 1 bit integer. for bools *)
      | Int8                    (* an 8 bit integer *)
      | Int32                   (* a 32 bit integer *)
      | Int64                   (* a 64 bit integer *)
      | Ptr of t                (* pointer; internally, we use typed pointers
                                 * to make it easier to generate some operations,
                                 * but these map to the LLVM opaque pointer
                                 * type in address space 0.
                                 *)
      | GCPtr of t              (* garbage collected pointer (address space 1) *)
      | Array of int * t        (* int = num elms *)
      | Struct of t list
      | Token                   (* unique returned by GC wrapped function calls *)
      | VarArg                  (* varargs; used for the GC intrinsics *)
      | Proto of t * t list     (* LLVM function type; for globals only *)

    (* return the LLVM textual representation of the type *)
    val llvmName : t -> string

    (* return a printable representation of the type; note that the result may not
     * be a LLVM type, since we use typed pointers internally.
     *)
    val toString : t -> string

    (* does a type map to the LLVM "ptr" type? *)
    val isLLVMPtr : t -> bool

    (* dereference a pointer type; returns `NONE` if the type is not a pointer *)
    val deref : t -> t option

    val gepTy : t * LLVMRep.var list -> t

    (* are two types equal? Note that this function does not differentiate between
     * different types of pointers.
     *)
    val same : t * t -> bool

  end = struct

    structure Rep = LLVMRep

    datatype t = datatype Rep.ty

    fun llvmName ty = (case ty
           of Void => "void"
            | Int1 => "i1"
            | Int8 => "i8"
            | Int32 => "i32"
            | Int64 => "i64"
            | Ptr _ => "ptr"
            | GCPtr _ => "ptr addrspace(1)"
            | Array(sz, ty) => String.concat [
                  "[", Int.toString sz, " x ", llvmName ty, "]"
                ]
            | Struct tys => String.concat [
                  "{", String.concatWithMap "," llvmName tys, "}"
                ]
            | Token => "token"
            | VarArg => "..."
            | Proto _ => "ptr"
          (* end case *))

    fun toString ty = (case ty
           of Ptr ty => toString ty ^ "*"
            | GCPtr ty => toString ty ^ " addrspace(1)*"
            | Array(sz, ty) => String.concat [
                  "[", Int.toString sz, " x ", toString ty, "]"
                ]
            | Struct tys => String.concat [
                  "{", String.concatWithMap "," toString tys, "}"
                ]
            | ty => llvmName ty
          (* end case *))

    fun isLLVMPtr (Ptr _) = true
      | isLLVMPtr (GCPtr _) = true
      | isLLVMPtr (Proto _) = true
      | isLLVMPtr _ = false

    fun deref ty = (case ty
           of Ptr ty => SOME ty
            | GCPtr ty => SOME ty
            | _ => NONE
          (* end case *))

    (* error checking/reporting is light here, we just care about
     * getting the right type for well-formed queries.
     *)
    fun gepTy (ptrTy, _::offsets) = let
          fun lp ([], ty) = ty
            | lp (v::vs, ty) = (case (ty, v)
                 of (Struct us, Rep.IConst(_, i)) => lp(vs, List.nth(us, IntInf.toInt i))
                  | (Array(_, u), _) => u
                  | _ => ty    (* optimistic. it's either ty or an error *)
                (* end case *))
          in
            case ptrTy
             of Ptr ty => Ptr(lp(offsets, ty)) (* step through the pointer *)
              | GCPtr ty => GCPtr(lp(offsets, ty))
              | _ => raise Fail(concat[
                    "gepTy (", toString ptrTy, "); argument must be pointer type"
                  ])
            (* end case *)
          end

    fun same (ty1, ty2) = (case (ty1, ty2)
           of (Void, Void) => true
            | (Int1, Int1) => true
            | (Int8, Int8) => true
            | (Int32, Int32) => true
            | (Int64, Int64) => true
            | (Ptr _, Ptr _) => true
            | (GCPtr _, GCPtr _) => true
            | (Array(n1, ty1), Array(n2, ty2)) => (n1 = n2) andalso same (ty1, ty2)
            | (Struct tys1, Struct tys2) => ListPair.allEq same (tys1, tys2)
            | (Token, Token) => true
            | (VarArg, VarArg) => true
            | (Proto _, Proto _) => true
            | (Ptr _, Proto _) => true
            | (Proto _, Ptr _) => true
            | _ => false
          (* end case *))

  end
