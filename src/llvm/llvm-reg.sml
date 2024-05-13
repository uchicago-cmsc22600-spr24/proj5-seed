(* llvm-reg.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *)

structure LLVMReg : sig

    (* the abstract type of LLVM pseudo registers *)
    type t

    (* create a new LLVM register with the given name and type *)
    val newNamed : string * LLVMType.t -> t

    (* create a new register with a generated name *)
    val new : LLVMType.t -> t

    (* return the type of a register *)
    val typeOf : t -> LLVMType.t

    (* produce a string representation of a register *)
    val toString : t -> string

    (* are two registers the same? *)
    val same : t * t -> bool

  end = struct

    datatype t = datatype LLVMRep.reg

    val cnt = ref 1

    fun newNamed (name, ty) = let
          val id = !cnt
          in
            cnt := id + 1;
            Reg{name = LLVMRep.mangle name, ty = ty, id = id}
          end

    fun new ty = let
          (* we need a prefix to avoid the requirement that registers be defined
           * in numeric order.  We pick the prefix based on the type for better
           * readability.
           *)
          val prefix = (case ty
                 of LLVMRep.Int1 => "b"
                  | LLVMRep.Int8 => "n"
                  | LLVMRep.Int32 => "n"
                  | LLVMRep.Int64 => "n"
                  | LLVMRep.Ptr _ => "p"
                  | LLVMRep.GCPtr _ => "g"
                  | LLVMRep.Array _ => "a"
                  | LLVMRep.Struct _ => "s"
                  | LLVMRep.Token => "tok"
                  | LLVMRep.VarArg => "va"
                  | LLVMRep.Proto _ => "f"
                (* end case *))
          in
            newNamed (prefix, ty)
          end

    fun typeOf (Reg{ty, ...}) = ty

    fun toString (Reg{name, id, ...}) = concat["%", name, Int.toString id]

    fun same (Reg{id=a, ...}, Reg{id=b, ...}) = (a = b)

  end
