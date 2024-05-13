(* util.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Code generation utility functions.
 *)

structure Util : sig

    (* convert primitive types to LLVM types.  This conversion has the
     * effect of mapping Boxed and Mixed types to pointers and Unboxed
     * types to integers. *)
    val cvtType : PrimType.t -> LLVMType.t

    (* allocate parameters registers for a fragment; these will be in the same
     * order as the fragment's parameter variables.  Note that this function
     * should only be used for entry and join fragments; in other cases the
     * LLVM variables that hold the parameter values will be defined in the
     * predecessor block (or earlier).
     *)
    val getParamRegs : CFGFrag.t -> LLVMReg.t list

    (* get the LLVM type of the function being applied from a CFG application *)
    val getAppliedFunType : CFG.application -> LLVMType.t

  end = struct

    structure PTy = PrimType
    structure LTy = LLVMType

    val intTy = MMLRuntime.intTy
    val ptrTy = MMLRuntime.ptrTy

    fun cvtType PTy.RAW = LTy.Int64
      | cvtType PTy.CODE = ptrTy
      | cvtType (PTy.UNIFORM PTy.Unboxed) = intTy
      | cvtType (PTy.UNIFORM _) = ptrTy

    fun cvtVar x = LLVMReg.newNamed (CFGVar.nameOf x, cvtType (CFGVar.typeOf x))

    fun getParamRegs frag = List.map cvtVar (CFGFrag.paramsOf frag)

    fun getAppliedFunType (retTy, _, args) = LTy.Proto(
            cvtType retTy,
            List.map (fn arg => cvtType(CFGUtil.typeOfValue arg)) args)

  end
