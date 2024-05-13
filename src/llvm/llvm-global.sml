(* llvm-global.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *)

structure LLVMGlobal : sig

    (* the abstract type of LLVM global names *)
    type t

    (* define a global with the given type.  Note that globals are identified
     * by name, so two calls to this function with the same string argument
     * should have the same type to avoid problems in the generated LLVM code.
     * Note also that this call does **not** declare the name in the generated
     * LLVM code; it is used by other modules to create the names of functions,
     * string constants, etc.
     *)
    val new : string * LLVMType.t -> t

    (* return the name of the global (will include the "@" prefix) *)
    val nameOf : t -> string

    (* return the type of the global *)
    val typeOf : t -> LLVMType.t

  end = struct

    structure Rep = LLVMRep

    datatype t = datatype Rep.global

    fun new (name, ty) = Rep.Glob {
            name = Atom.atom("@" ^ Rep.mangle name),
            ty = ty
          }

    fun nameOf (Glob{name, ...}) = Atom.toString name

    fun typeOf (Glob{ty, ...}) = ty

  end
