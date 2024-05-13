(* cfg-funct.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Utility functions for CFG fragments.
 *)

structure CFGFunct : sig

    type t

    (* `new (retTy, frags)` creates a CFG function.  The `retTy` is the return
     * type of the function and `frags` is a non-empty list of fragments for
     * the function.  The first fragment in the list will be the entry fragment.
     *)
    val new : PrimType.t * CFGFrag.t list -> t

    (* return the entry fragment of the function *)
    val entryOf : t -> CFGFrag.t

    (* return the fragments of the function (not including the entry fragment) *)
    val fragmentsOf : t -> CFGFrag.t list

    (* return the name of the function (which is the name of its entry label) *)
    val nameOf : t -> string

    (* return the return type of the function *)
    val returnTypeOf : t -> PrimType.t

    (* return the type signature of the function: return type and parameter types *)
    val typeOf : t -> PrimType.t * PrimType.t list

  end = struct

    datatype t = datatype CFGRep.function

    fun new (ty, entry::frags) = let
          val CFGRep.FRAG{lab, ...} = entry
          val function = FUN{retTy=ty, entry=entry, frags=frags}
          in
            CFGLabel.setKind (lab, CFGLabel.LK_Entry function);
            function
          end
      | new _ = raise Fail "CFGFunct.new called with empty fragment list"

    fun entryOf (FUN{entry, ...}) = entry

    fun fragmentsOf (FUN{frags, ...}) = frags

    fun nameOf (FUN{entry=CFGRep.FRAG{lab, ...}, ...}) = CFGLabel.nameOf lab

    fun returnTypeOf (FUN{retTy, entry, ...}) = retTy

    fun typeOf (FUN{retTy, entry, ...}) =
          (retTy, List.map CFGVar.typeOf (CFGFrag.paramsOf entry))

  end
