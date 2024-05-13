(* llvm-block.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * LLVM basic blocks.  This module encapsulates most of the operations for emitting
 * LLVM instructions.  Most instructions are added to the end of the specified block.
 * The one exception are phi instructions, which are always added to the beginning
 * of the block (even when other instructions have already been added).
 *
 * The last instruction in a block must be a control-flow instruction.  Once such an
 * instruction is emitted, the block is marked as closed and additional instructions
 * cannot be added.
 *)

structure LLVMBlock : sig

    (* LLVM Basic block *)
    type t

    (* the abstract representation of a φ node for a block *)
    type phi

    type var = LLVMVar.t

    (* exception raised if an attempt is made to add instructions to a closed block *)
    exception Closed

    (* get the label of the block *)
    val labelOf : t -> LLVMLabel.t

    (* is a block closed?  I.e., has a terminating control-flow instruction
     * been added to it?
     *)
    val isClosed : t -> bool

    (* define a φ node for the given LLVM register.  The definition of the
     * φ node is constructed incrementally using the `addPhiDef` function.
     *)
    val emitPhi : t * LLVMReg.t -> phi

    (* add an incoming definition to a φ node.  Note that it is permissible
     * to call this function after the block has been closed.
     *)
    val addPhiDef : phi * LLVMVar.t * t -> unit

    (* get the variable defined by a φ node *)
    val phiToVar : phi -> LLVMVar.t

    (* emit a comment (for debugging); the comment should not have a terminating "\n" *)
    val emitComment : t * string -> unit

    (* typecast the variable to the specified type *)
    val emitCast : t * LLVMType.t * var -> var

    (** control flow instructions; these close off the block **)

    (* return instruction with optional return value *)
    val emitReturn  : t * var option -> unit
    (* unconditional branch to label *)
    val emitBr      : t * LLVMLabel.t -> unit
    (* unconditional branch to block *)
    val emitBr'     : t * t -> unit
    (* conditional branch to labels *)
    val emitCondBr  : t * var * LLVMLabel.t * LLVMLabel.t -> unit
    (* conditional branch to blocks *)
    val emitCondBr' : t * var * t * t -> unit

    (* Emit a function call.  Note that for calls that cannot trigger GC,
     * the list of live variables can be empty.
     *)
    val emitCall : t * {
            func : var,                 (* the function being called *)
            args : var list,            (* the function arguments *)
            live : var list             (* the variables that are live after
                                         * the call.
                                         * NOTE: these should *not* include
                                         * the result of the call!
                                         *)
          } -> {
            ret : var option,           (* the result register (NONE for void) *)
            live : var list             (* the renamed live list *)
          }

    (* tail calls.  This function will generate both the tail call and the
     * LLVM return instruction that must immediately follow a tail call.
     * Thus, this function terminates the block.
     *)
    val emitTailCall : t * {
            func : var,                 (* the function being called *)
            args : var list             (* the function arguments *)
          } -> unit

    (** integer arithmetic **)

    val emitAdd : t * var * var -> var          (* signed integer addition *)
    val emitSub : t * var * var -> var          (* signed integer subtraction *)
    val emitMul : t * var * var -> var          (* signed integer multiplication *)
    val emitDiv : t * var * var -> var          (* signed integer division *)
    val emitRem : t * var * var -> var          (* signed integer remainder *)
    val emitAShftR : t * var * var -> var       (* arithmetic right shift *)
    val emitShftL : t * var * var -> var        (* left shift *)
    val emitOr : t * var * var -> var           (* bitwise inclusive or *)
    val emitXor : t * var * var -> var          (* bitwise exclusive or *)
    val emitAnd : t * var * var -> var          (* bitwise and *)
    val emitXorBit : t * var * var -> var       (* 1-bit exclusive or; used for negation *)

    (** comparisons **)

    val emitEqu : t * var * var -> var          (* equality comparison *)
    val emitNEq : t * var * var -> var          (* inequality comparison *)
    val emitLte : t * var * var -> var          (* signed less-than-or-equal comparison *)
    val emitLt  : t * var * var -> var          (* signed less-than comparison *)
    val emitGt  : t * var * var -> var          (* signed greater-than comparison *)
    val emitGte : t * var * var -> var          (* signed greater-than-or-equal comparison *)
    val emitULt : t * var * var -> var          (* unsigned less-than comparison *)

    (** memory operations **)

    (* `addr (blk, elemTy, base, offset)` returns a pointer to the address
     * `base + sizeof(elemTy)*offset`.
     *)
    val emitAddr : t * LLVMType.t * var * var -> var

    (* `emitLd (blk, ty, addr)` emits a load instruction that loads a value of the
     * specified type.
     *)
    val emitLd  : t * LLVMType.t * var -> var

    (* `emitSt (blk, addr, v)` emits a store operation ( <addr>[0] = <v> ) *)
    val emitSt  : t * {addr : var, value : var} -> unit

  end = struct

    structure Rep = LLVMRep
    structure Ty = LLVMType
    structure Var = LLVMVar

    datatype t = datatype Rep.block
    datatype var = datatype Rep.var

    (* we identify PHI nodes by their block and lhs register *)
    datatype phi = PHI of t * Rep.reg

    exception Closed

    (* get the owner of a block *)
    fun ownerOf (Blk{owner, ...}) = owner

    (* close off a block *)
    fun close (Blk{phis, body, closed, ...}) = (
          phis := List.rev (!phis);
          body := List.rev (!body);
          closed := true)

    fun labelOf (Blk{name, ...}) = name

    fun isClosed (Blk{closed, ...}) = !closed

    fun addInstr (Blk{body, closed=ref false, ...}, instr) = (body := instr :: !body)
      | addInstr _ = raise Closed

    val newReg = LLVMReg.new

    (* generic function for emitting instructions that return results.  Arguments are
     *    ty      -- result type of operation
     *    rator   -- operation
     *    mk      -- function to process arguments
     *)
    fun emitInstr ty rator mk args = let
          val res = newReg ty
          val (blk, args) = mk args
          val instr = Rep.Instr{result = SOME res, rator = rator, args = args}
          in
            addInstr (blk, instr);
            Rep.VReg res
          end

    fun emitInstr' ty rator (blk, args) = let
          val res = newReg ty
          val instr = Rep.Instr{result = SOME res, rator = rator, args = args}
          in
            addInstr (blk, instr);
            Rep.VReg res
          end

    fun emitNoResI (blk, rator, args) = let
          val instr = Rep.Instr{result = NONE, rator = rator, args = args}
          in
            addInstr (blk, instr)
          end

    fun mkArgs1 (blk, a) = (blk, [a])
    fun mkArgs2 (blk, a, b) = (blk, [a, b])

    fun emitPhi (blk as Blk{phis, closed=ref false, ...}, reg) = (
          phis := Rep.Phi(reg, []) :: !phis;
          PHI(blk, reg))
      | emitPhi _ = raise Closed

    fun addPhiDef (PHI(Blk{phis, ...}, lhs), reg, Blk{name, ...}) = let
          (* find the correct phi node in the `phis` list and update it *)
          fun find ([], _) = raise Fail "impossible"
            | find ((phi as Rep.Phi(lhs', rhs))::phis, prefix) =
                if LLVMReg.same(lhs, lhs')
                  then List.revAppend(prefix, Rep.Phi(lhs, (reg, name)::rhs)::phis)
                  else find (phis, phi :: prefix)
          in
            phis := find (!phis, [])
          end

    fun phiToVar (PHI(_, r)) = Rep.VReg r

    fun emitComment (blk, msg) = emitNoResI (blk, Rep.CommentOp, [Rep.Comment msg])

    fun cast (blk, ty, var) = emitInstr' ty Rep.CastOp (blk, [var])

    fun alwaysCast (blk, ty, var) =
          if Ty.isLLVMPtr ty andalso Ty.isLLVMPtr(Var.typeOf var)
            (* convert to int and then to pointer to avoid address-space issues *)
            then cast (blk, ty, cast (blk, Ty.Int64, var))
            else cast (blk, ty, var)

    fun emitCast (blk, ty, var) = if Ty.same(ty, Var.typeOf var)
          then var
          else alwaysCast (blk, ty, var)

  (* make arguments to i64 binary operators (including comparisons) *)
    fun mkI64Args2 (blk, a, b) = let
          fun mkI64 arg = if Ty.same(Ty.Int64, Var.typeOf arg)
                then arg
                else alwaysCast (blk, Ty.Int64, arg)
          in
            (blk, [mkI64 a, mkI64 b])
          end

    (* integer arithmetic *)
    local
      fun emitInstrI64 oper = emitInstr Ty.Int64 oper mkI64Args2
    in
    val emitAdd = emitInstrI64 Rep.AddOp
    val emitSub = emitInstrI64 Rep.SubOp
    val emitMul = emitInstrI64 Rep.MulOp
    val emitDiv = emitInstrI64 Rep.DivOp
    val emitRem = emitInstrI64 Rep.RemOp
    val emitAShftR = emitInstrI64 Rep.AShftROp
    val emitShftL = emitInstrI64 Rep.ShftLOp
    val emitOr = emitInstrI64 Rep.OrOp
    val emitXor = emitInstrI64 Rep.XorOp
    val emitAnd = emitInstrI64 Rep.AndOp
    end (* local *)

    (* single-bit xor; used to implement logical negation *)
    val emitXorBit = emitInstr Ty.Int1 Rep.XorOp mkArgs2

    (* integer comparisons *)
    local
      fun emitInstrI64 oper = emitInstr Ty.Int1 oper mkI64Args2
    in
    val emitLte = emitInstrI64 Rep.LteOp
    val emitLt  = emitInstrI64 Rep.LtOp
    val emitGt  = emitInstrI64 Rep.GtOp
    val emitGte = emitInstrI64 Rep.GteOp
    val emitULt = emitInstrI64 Rep.ULtOp
    end (* local *)

  (* equality tests *)
    local
    (* For equality tests, we might have pointer/int comparisons because of nil.
     * This function checks for that case and adds a cast if required.  We also
     * check for address-space type conflicts.
     *)
      fun castEqArgs (blk, a, b) = let
            val aTy = Var.typeOf a
            val bTy = Var.typeOf b
            in
              if Ty.same(aTy, bTy)
                then (blk, [a, b])
                else (case (aTy, bTy)
                   of (Ty.Int64, _) => (blk, [a, emitCast(blk, Ty.Int64, b)])
                    | (_, Ty.Int64) => (blk, [emitCast(blk, Ty.Int64, a), b])
                    | _ => raise Fail(concat[
                          "equality between ", Ty.toString aTy, " and ", Ty.toString bTy
                        ])
                  (* end case *))
            end
    in
    val emitEqu = emitInstr Ty.Int1 Rep.EquOp castEqArgs
    val emitNEq = emitInstr Ty.Int1 Rep.NEqOp castEqArgs
    end (* local *)

  (* memory operations *)
    fun emitLd (blk, ty, ptr) = let
          val SOME resTy = Ty.deref (Var.typeOf ptr)
          val res = emitInstr' resTy Rep.LoadOp (blk, [ptr])
          in
            if Ty.same(resTy, ty)
              then res
              else emitCast (blk, ty, res)
          end

    fun emitSt (blk, {value, addr}) = let
          val addrTy = Var.typeOf addr
          val SOME pointeeTy = Ty.deref addrTy
          val valueTy = Var.typeOf value
          val addr = if Ty.same(valueTy, pointeeTy)
                then addr
                else emitCast (blk, addrTy, addr)
          in
            emitNoResI (blk, Rep.StoreOp, [value, addr])
          end

    fun emitAddr (blk, elemTy, base, offset) = let
          val elemPtrTy = Ty.Ptr elemTy
          val base = emitCast (blk, elemPtrTy, base)
          val resTy = Ty.gepTy (elemPtrTy, [offset])
          in
          (* NOTE that we're skipping the first offset. *)
            emitInstr' resTy Rep.GetElemPtrOp (blk, [base, offset])
          end

  (* See documentation about gc.statepoint for the details. *)
    fun emitCall (blk, {func, args, live}) = let
          val Ty.Proto(retT, paramTys) = Var.typeOf func
        (* add casts for arguments that are ints, where the expected type is a pointer *)
          val args = let
                fun addCast (x, ty as Ty.GCPtr _) = if Ty.same(Var.typeOf x, ty)
                      then x
                      else emitCast(blk, ty, x)
                (* also need casts when passing metadata of type [n x i64]* to sool_new, which expects i64* *)
                  | addCast (x, ty as Ty.Ptr _) = (case Var.typeOf x
                      of Ty.Ptr(Ty.Array _) => emitCast(blk, ty, x)
                       | _ => x
                     (* end case *))
                  | addCast (x, _) = x
                in
                  ListPair.map addCast (args, paramTys)
                end
          val tok = emitInstr' Rep.Token (Rep.CallOp live) (blk, func :: args)
          val ret = (case retT
                 of Ty.Void => NONE
                  | _ => SOME(emitInstr' retT Rep.RetValOp (blk, [tok]))
                (* end case *))
          fun cvt (inputVar, (i, rest)) = (case Var.typeOf inputVar
                 of ty as Ty.GCPtr _ => let
                    (* for live variables that are garbage collected, we have to rename *)
                      val offset = Var.const(Ty.Int32, Int.toLarge i)
                      val output = emitInstr' ty Rep.RelocOp (blk, [tok, offset, offset])
                      in
                        (i+1, output::rest)
                      end
                  | _ => (i, inputVar::rest)
                (* end case *))
          val startIdx = 5 + (List.length args) + 2 (* magic *)
          val (_, cvtd) = List.foldl cvt (startIdx, []) live
          in
            { ret = ret, live = List.rev cvtd }
          end

     fun emitTailCall (blk, {func, args}) = (case Var.typeOf func
           of Ty.Proto(retT, paramTys) => let
                val _ = emitComment (blk, concat [
                        "emitTailCall: ", Var.toString func, " (",
                        String.concatWithMap ", " Var.toString args, ")"
                      ])
              (* add casts for arguments that are ints, where the expected type is a pointer *)
                val args = let
                      fun addCast (x, ty as Ty.Ptr _) = emitCast (blk, ty, x)
                        | addCast (x, _) = x
                      in
                        ListPair.map addCast (args, paramTys)
                      end
                val res = emitInstr' retT Rep.TailCallOp (blk, func :: args)
                in
                  case retT
                   of Ty.Void => emitNoResI (blk, Rep.Return, [])
                    | _ => let
                      (* make sure that the argument to the return matches the
                       * function's type
                       *)
                        val res = emitCast (blk, LLVMFunc.returnTyOf(ownerOf blk), res)
                        in
                          emitNoResI (blk, Rep.Return, [res])
                        end
                  (* end case *);
                  close blk
                end
            | ty => raise Fail(concat[
                  "emitTailCall: typeOf(", Var.toString func, ") = ",
                  Ty.toString ty
                ])
          (* end case *))

 (* terminators, which close the block *)

    fun emitReturn (blk, NONE) = (emitNoResI (blk, Rep.Return, []); close blk)
      | emitReturn (blk, SOME var) = let
        (* make sure that the argument to the return matches the function's type *)
          val var = emitCast (blk, LLVMFunc.returnTyOf(ownerOf blk), var)
          in
            emitNoResI (blk, Rep.Return, [var]);
            close blk
          end

    fun emitBr (blk, lab) = (
          emitNoResI (blk, Rep.Goto, [Rep.Label lab]);
          close blk)

    fun emitCondBr (blk, cond, trueLab, falseLab) = (
          emitNoResI (blk, Rep.CondBr, [cond, Rep.Label trueLab, Rep.Label falseLab]);
          close blk)

    (* branches specified by target blocks, instead of labels *)
    fun emitBr' (blk, Blk{name, ...}) = emitBr(blk, name)
    fun emitCondBr' (blk, cond, Blk{name=l1, ...}, Blk{name=l2, ...}) =
          emitCondBr (blk, cond, l1, l2)

  end
