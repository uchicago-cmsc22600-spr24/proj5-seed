(* llvm-func.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Support for LLVM functions.
 *)

structure LLVMFunc : sig

    (* the abstract representation of an LLVM function *)
    type t

    (* `new m {tailCC, retTy, name, params}` creates a new function in the module `m`.
     * The return type of the function is given by `retTy`.  Note that this operation
     * creates a new global variable that names the function.
     *)
    val new : LLVMModule.t -> {
            tailCC : bool,              (* if true, then use the "tailcc" calling convention *)
            retTy : LLVMType.t,         (* the function's return type *)
            name : string,              (* the function's name *)
            params : LLVMReg.t list     (* the pseudo registers assigned as parameters *)
          } -> t

    (* return the calling convention of the function *)
    val ccOf : t -> string

    (* returns the name of the function, which is a global name *)
    val nameOf : t -> LLVMGlobal.t

    (* returns the list of parameters for the function *)
    val paramsOf : t -> LLVMReg.t list

    (* get the entry block of the function *)
    val entryOf : t -> LLVMRep.block

    (* get the return type of a function *)
    val returnTyOf : t -> LLVMType.t

    (* `newBlock (func, lab)` creates a new block in the function `func` with
     * `lab` as the name of the block's label.
     *)
    val newBlock : t * string -> LLVMRep.block

  end = struct

    structure Rep = LLVMRep

    datatype t = datatype Rep.func

    fun newBlk (f, lab) = Rep.Blk{
            owner = f,
            name = LLVMLabel.new lab,
            phis = ref[],
            body = ref[],
            closed = ref false
          }

    fun nameOf (Func{name, ...}) = name

    fun paramsOf (Func{params, ...}) = params

    fun entryOf (Func{entry, ...}) = valOf (!entry)

    fun newBlock (f as Func{body, ...}, lab) = let
          val blk = newBlk (f, lab)
          in
            body := blk :: !body;
            blk
          end

    fun new (Rep.Module{funs, ...}) {tailCC, retTy, name, params} = let
          val ty = LLVMType.Proto(retTy, List.map LLVMReg.typeOf params)
          val entryBlk = ref NONE
          val func = Rep.Func {
                  (* hack to make sure that the entry function uses the standard
                   * calling convention.
                   *)
                  exported = (name = "_mml_entry"),
                  name = LLVMGlobal.new(name, ty),
                  params = params,
                  entry = entryBlk,
                  body = ref[]
                }
          in
            entryBlk := SOME(newBlk (func, "entry"));
            funs := func :: !funs;
            func
          end

    fun returnTyOf (Func{name, ...}) = (case LLVMGlobal.typeOf name
           of LLVMType.Proto(retTy, _) => retTy
            | _ => raise Fail "non-function type for function"
          (* end case *))

    fun ccOf (Func{exported, ...}) = if exported then "" else "fastcc"

  end
