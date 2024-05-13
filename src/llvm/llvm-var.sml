(* llvm-var.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *)

structure LLVMVar : sig

    (* LLVM variables, which include constants, globals, and locals *)
    type t

    (* constant value *)
    val const  : LLVMType.t * IntInf.int -> t
    (* register value (i.e., local variable) *)
    val reg    : LLVMReg.t -> t
    (* global variable *)
    val global : LLVMGlobal.t -> t

    (* return the type of a variable *)
    val typeOf : t -> LLVMType.t

    (* map a variable to a string *)
    val toString : t -> string

  end = struct

    structure Rep = LLVMRep

    datatype t = datatype Rep.var

    fun const (ty, value) = IConst(ty, value)

    val reg = VReg

    val global = Global

    fun typeOf (VReg(Rep.Reg{ty,...})) = ty
      | typeOf (IConst(ty, _)) = ty
      | typeOf (Global(Rep.Glob{ty,...})) = ty
      | typeOf _ = raise Fail "cannot ask the type of a Label or Comment var."

    fun toString (VReg reg) = LLVMReg.toString reg
      | toString (IConst(_, value)) =
          if value < 0
            then "-" ^ IntInf.toString(~value)
            else IntInf.toString value
      | toString (Global glob) = LLVMGlobal.nameOf glob
      | toString (Label lab) = LLVMLabel.nameOf lab
      | toString (Comment msg) = msg

  end
