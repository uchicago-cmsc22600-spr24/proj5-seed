(* mml-runtime.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * LLVM definitions for the MiniML runtime system.
 *)

structure MMLRuntime : sig

    (* the type of integers *)
    val intTy : LLVMType.t
    (* the type of heap pointers *)
    val ptrTy : LLVMType.t

    (* the LLVM globals that represent runtime system functions *)
    val funAlloc : LLVMGlobal.t
    val funConcat : LLVMGlobal.t
    val funExit : LLVMGlobal.t
    val funFail : LLVMGlobal.t
    val funPrint : LLVMGlobal.t
    val funStrChr : LLVMGlobal.t

    (* generate the declarations of the globals into the given module *)
    val declareRuntimeGlobals : LLVMModule.t -> unit

    (* map CFG runtime names to their LLVM counterparts *)
    val lookup : Runtime.t -> LLVMGlobal.t

  end = struct

    structure LG = LLVMGlobal
    structure LTy = LLVMType
    structure R = Runtime

    val intTy = LTy.Int64
    val ptrTy = LTy.GCPtr LTy.Int64

    (* the LLVM globals that represent runtime system functions *)
    local
      fun new (name, ty, tys) = LG.new(name, LTy.Proto(ty, tys))
    in
    val funAlloc = new ("_mml_alloc", ptrTy, [intTy])
    val funConcat = new (R.nameOf R.funConcat, ptrTy, [ptrTy, ptrTy])
    val funExit = new (R.nameOf R.funExit, LTy.Void, [intTy])
    val funFail = new (R.nameOf R.funFail, LTy.Void, [ptrTy])
    val funPrint = new (R.nameOf R.funPrint, LTy.Void, [ptrTy])
    val funStrChr = new ("_mml_str_chr", ptrTy, [intTy])
    end

    (* generate the declarations of the globals into the given module *)
    fun declareRuntimeGlobals module = List.app
          (fn g => LLVMModule.declareExtern(module, g)) [
              funAlloc, funConcat, funExit, funFail, funPrint, funStrChr
            ]

    (* lookup for runtime functions; since there are so few of them, a list-based
     * search is fine.
     *)
    val lookup = let
          val mapping = [
                  (R.funConcat, funConcat),
                  (R.funExit, funExit),
                  (R.funFail, funFail),
                  (R.funPrint, funPrint),
                  (R.funStrChr, funStrChr)
                ]
          in
            fn cf => (case List.find (fn (cf', _) => R.same(cf, cf')) mapping
                 of SOME(_, glob) => glob
                  | NONE => raise Fail(concat[
                        "unknown runtime function '", R.nameOf cf, "'"
                      ])
                (* end case *))
          end

  end
