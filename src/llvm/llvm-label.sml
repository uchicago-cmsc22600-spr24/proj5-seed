(* llvm-label.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *)

structure LLVMLabel : sig

    (* the abstract type of LLVM labels *)
    type t

    (* generated a new label with the given name *)
    val new : string -> t

    (* return the label's name (does not include "%" prefix) *)
    val nameOf : t -> string

  end = struct

    structure Rep = LLVMRep

    datatype t = datatype Rep.label

    fun new name = Rep.Lab{name = Rep.mangle name, id = Stamp.new()}

    fun nameOf (Rep.Lab{name, id}) = concat[name, "_", Stamp.toString id]

  end
