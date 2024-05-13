(* arith-gen.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * The MiniML `Int` type is represented as a tagged 63-bit number (i.e., the value
 * `n` is represented as `2*n+1`).  This module implements the translation of MiniML
 * arithmetic operations on tagged integers to 64-bit LLVM operations.
 *)

structure ArithGen : sig

    (* if the variable is a 1-bit boolean, then convert it to a MiniML boolean.  This
     * function acts as the identity on vars with other types.
     *)
    val toBool : LLVMBlock.t * LLVMVar.t -> LLVMVar.t

    (* if the variable is a MiniML boolean, then convert it to a 1-bit boolean.  This
     * function expects either a 1-bit or 64-bit argument.
     *)
    val toBit : LLVMBlock.t * LLVMVar.t -> LLVMVar.t

    (* `add (blk, env, x, y)` emits code for the tagged addtion `x+y` into the
     * block `blk`. The map `env` is used to lookup the bindings of variable arguments.
     *)
    val add : LLVMBlock.t * Env.t * CFG.value * CFG.value -> LLVMVar.t

    (* `sub (blk, env, x, y)` emits code for the tagged subtraction `x-y` into the
     * block `blk`. The map `env` is used to lookup the bindings of variable arguments.
     *)
    val sub : LLVMBlock.t * Env.t * CFG.value * CFG.value -> LLVMVar.t

    (* `mul (blk, env, x, y)` emits code for the tagged addtion `x*y` into the
     * block `blk`. The map `env` is used to lookup the bindings of variable arguments.
     *)
    val mul : LLVMBlock.t * Env.t * CFG.value * CFG.value -> LLVMVar.t

    (* `quot (blk, env, x, y)` emits code for the tagged division `x/y` into the
     * block `blk`. The map `env` is used to lookup the bindings of variable arguments.
     *)
    val quot : LLVMBlock.t * Env.t * CFG.value * CFG.value -> LLVMVar.t

    (* `rem (blk, env, x, y)` emits code for the tagged remainder `x%y` into the
     * block `blk`. The map `env` is used to lookup the bindings of variable arguments.
     *)
    val rem : LLVMBlock.t * Env.t * CFG.value * CFG.value -> LLVMVar.t

    (* convert an integer literal to its runtime representation *)
    val intToRep : IntInf.int -> IntInf.int

    (* comparisons; the equality operations work on both integers and pointers.
     * the resulting variable has type Int1.
     *)
    val lt  : LLVMBlock.t * Env.t * CFG.value * CFG.value -> LLVMVar.t
    val lte : LLVMBlock.t * Env.t * CFG.value * CFG.value -> LLVMVar.t
    val equ : LLVMBlock.t * Env.t * CFG.value * CFG.value -> LLVMVar.t
    val neq : LLVMBlock.t * Env.t * CFG.value * CFG.value -> LLVMVar.t

    (* unsigned comparison *)
    val ult : LLVMBlock.t * Env.t * CFG.value * CFG.value -> LLVMVar.t

  end = struct

    structure LB = LLVMBlock
    structure LV = LLVMVar
    structure LTy = LLVMType

    fun withVar (env, x, f) = f (Env.lookup (env, x))

    fun intToRep (n : IntInf.int) = n + n + 1

    fun iconst n = LV.const(LLVMType.Int64, n)

    (* encoding of True and False *)
    val trueVal = iconst 3
    val falseVa = iconst 1

    fun dec (blk, x) = LB.emitSub (blk, x, iconst 1)
    fun inc (blk, x) = LB.emitAdd (blk, x, iconst 1)
    fun div2 (blk, x) = LB.emitAShftR (blk, x, iconst 1)
    fun mul2 (blk, x) = LB.emitShftL (blk, x, iconst 1)

    (* if the variable is a 1-bit boolean, then convert it to a MiniML boolean *)
    fun toBool (blk, v) = (case LV.typeOf v
           of LTy.Int1 => (* ((int)v << 1)+1 *)
                LB.emitAdd(blk,
                  LB.emitShftL(blk, LB.emitCast(blk, LTy.Int64, v), iconst 1),
                  iconst 1)
            | _ => v
          (* end case *))

    (* if the variable is a MiniML boolean, then convert it to a 1-bit boolean *)
    fun toBit  (blk, v) = (case LV.typeOf v
           of LTy.Int64 => (* (v == true) *)
                LB.emitEqu(blk, v, trueVal)
            | LTy.Int1 => v
            | ty => raise Fail("toBit: unexepected type " ^ LTy.toString ty)
          (* end case *))

    (* emit code for tagged addition *)
    fun add (blk, env, CFG.VAR x, CFG.VAR y) =
          withVar (env, x, fn x' =>
          withVar (env, y, fn y' =>
            LB.emitAdd (blk, dec (blk, x'), y')))
      | add (blk, env, CFG.VAR x, CFG.INT a) =
          withVar (env, x, fn x' =>
            LB.emitAdd (blk, x', iconst(intToRep a - 1)))
      | add (blk, env, CFG.INT b, CFG.VAR y) =
          withVar (env, y, fn y' =>
            LB.emitAdd (blk, iconst(intToRep b - 1), y'))
      | add (blk, _, CFG.INT b, CFG.INT a) = iconst(intToRep(b+a))
      | add _ = raise Fail "bogus arguments to add"

    fun sub (blk, env, CFG.VAR x, CFG.VAR y) =
          withVar (env, x, fn x' =>
          withVar (env, y, fn y' =>
            inc (blk, LB.emitSub (blk, x', y'))))
      | sub (blk, env, CFG.VAR x, CFG.INT b) =
          withVar (env, x, fn x' =>
            LB.emitSub (blk, x', iconst(intToRep b - 1)))
      | sub (blk, env, CFG.INT a, CFG.VAR y) =
          withVar (env, y, fn y' =>
            LB.emitSub (blk, iconst(intToRep a + 1), y'))
      | sub (blk, _, CFG.INT a, CFG.INT b) = iconst(intToRep(a-b))
      | sub _ = raise Fail "bogus arguments to sub"

    fun mul (blk, env, CFG.VAR x, CFG.VAR y) =
          withVar (env, x, fn x' =>
          withVar (env, y, fn y' =>
            inc (blk, LB.emitMul (blk, dec (blk, x'), div2 (blk, y')))))
      | mul (blk, env, CFG.VAR x, CFG.INT b) =
          withVar (env, x, fn x' =>
            inc (blk, LB.emitMul (blk, dec (blk, x'), iconst b)))
      | mul (blk, env, CFG.INT a, CFG.VAR y) =
          withVar (env, y, fn y' =>
            inc (blk, LB.emitMul (blk, iconst a, dec (blk, y'))))
      | mul (blk, _, CFG.INT a, CFG.INT b) = iconst(intToRep(a*b))
      | mul _ = raise Fail "bogus arguments to mul"

    fun quot (blk, env, CFG.VAR x, CFG.VAR y) =
          withVar (env, x, fn x' =>
          withVar (env, y, fn y' =>
            inc (blk, mul2 (blk, LB.emitDiv (blk, div2 (blk, x'), div2 (blk, y'))))))
      | quot (blk, env, CFG.VAR x, CFG.INT b) =
          withVar (env, x, fn x' =>
            inc (blk, mul2 (blk, LB.emitDiv (blk, div2 (blk, x'), iconst b))))
      | quot (blk, env, CFG.INT a, CFG.VAR y) =
          withVar (env, y, fn y' =>
            inc (blk, mul2 (blk, LB.emitDiv (blk, iconst a, div2 (blk, y')))))
      | quot (blk, _, CFG.INT a, CFG.INT 0) =
        (* this instruction should never get executed, ut we need something *)
          LB.emitDiv (blk, iconst a, iconst 0)
      | quot (blk, _, CFG.INT a, CFG.INT b) = iconst(intToRep(IntInf.quot(a, b)))
      | quot _ = raise Fail "bogus arguments to quot"

    fun rem (blk, env, CFG.VAR x, CFG.VAR y) =
          withVar (env, x, fn x' =>
          withVar (env, y, fn y' =>
            inc (blk, mul2 (blk, LB.emitRem (blk, div2 (blk, x'), div2 (blk, y'))))))
      | rem (blk, env, CFG.VAR x, CFG.INT b) =
          withVar (env, x, fn x' =>
            inc (blk, mul2 (blk, LB.emitRem (blk, div2 (blk, x'), iconst b))))
      | rem (blk, env, CFG.INT a, CFG.VAR y) =
          withVar (env, y, fn y' =>
            inc (blk, mul2 (blk, LB.emitRem (blk, iconst a, div2 (blk, y')))))
      | rem (blk, _, CFG.INT a, CFG.INT 0) =
        (* this instruction should never get executed, ut we need something *)
          LB.emitRem (blk, iconst a, iconst 0)
      | rem (blk, _, CFG.INT a, CFG.INT b) = iconst(intToRep(IntInf.rem(a, b)))
      | rem _ = raise Fail "bogus arguments to rem"

    (* comparisons; the equality operations work on both integers and pointers *)
    local
      fun withVal (env, v, f : LV.t -> LV.t) = (case v
             of CFG.VAR x => withVar (env, x, f)
              | CFG.INT n => f (iconst(intToRep n))
              | CFG.STRING s => f (LV.global(Env.stringLit(env, s)))
              | _ => raise Fail "bogus value argument to primop"
            (* end case *))
      fun emitCmp emit (blk, env, x, y) =
            withVal (env, x, fn x' =>
            withVal (env, y, fn y' =>
              emit (blk, x', y')))
      fun emitEqu emit (blk, env, x, y) =
            withVal (env, x, fn x' =>
            withVal (env, y, fn y' =>
              (* for equality, we need to ensure that booleans are lifted to 64-bit *)
              emit (blk, toBool(blk, x'), toBool(blk, y'))))
    in
    val lt  = emitCmp LB.emitLt
    val lte = emitCmp LB.emitLte
    val equ = emitEqu LB.emitEqu
    val neq = emitEqu LB.emitNEq
    val ult = emitCmp LB.emitULt
    end (* local *)

  end
