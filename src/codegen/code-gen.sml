(* code-gen.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * The main code generation module.
 *)

structure CodeGen : sig

    (* `gen (base, prog)` generates LLVM assembly code into the file `base.ll` *)
    val gen : string * CFG.program -> unit

  end = struct

    structure P = Prim
    structure PC = PrimCond
    structure AG = ArithGen
    structure LF = LLVMFunc
    structure LB = LLVMBlock
    structure LV = LLVMVar
    structure LTy = LLVMType
    structure RT = MMLRuntime
    structure Info = CodeGenInfo

    (* the basic LLVM types that we use.  Note that when we are unsure
     * about the type of a MiniML value, the we give it the `anyTy` so
     * that the GC will be aware of it.
     *)
    val anyTy = LTy.Ptr LTy.Int64
    val intTy = LTy.Int64

    (* convert an integer index to an LLVMVar.t representing it as a 32-bit integer constant *)
    fun index i = LV.const(LTy.Int32, IntInf.fromInt i)

    fun i64const n = LV.const (LTy.Int64, n)

    (* a flag for marking fragments that have already been visited *)
    local
      val {getFn, setFn} = CFGFrag.newFlag ()
    in
    val visited = getFn
    fun markVisited frag = setFn(frag, true)
    end (* local *)

    (* a property to associate a list of φ nodes with a join fragment *)
    val {setFn = setPhisForFrag, getFn = getPhisForFrag, ...} =
          CFGFrag.newProp (fn _ => ([] : LB.phi list))

    (* convert CFG values to LLVM *)
    fun cvtVal env v = (case v
           of CFG.VAR x => let
                val blk = Env.block env
                val v = Env.lookup(env, x)
                in
                  LB.emitCast(blk, Util.cvtType(CFGVar.typeOf x), v)
                end
            | CFG.INT n => i64const (AG.intToRep n)
            | CFG.STRING s => LV.global(Env.stringLit(env, s))
            | CFG.CODE lab => LV.global(Info.funcGlobal lab)
          (* end case *))

    (* `emitCall (env, ppt, f, args)` emits the function call `f (args)`, while
     * handling the bookkeeping related to managing the live variables at the
     * program point `ppt`.  It returns the pair `(ret, env)`, where `ret` is the
     * optional return value of the call and `env` is the updated environment.
     *)
    fun emitCall (env, ppt, f, args) = let
          (* get the live variables at this program point *)
          val live = Info.getLive ppt
          (* emit the call *)
          val {ret, live=live'} = LB.emitCall (Env.block env, {
                  func = f,
                  args = args,
                  live = List.map (fn x => Env.lookup(env, x)) live
                })
          (* rename the live variables in environment *)
          val env = ListPair.foldlEq
                (fn (x, x', env) => Env.bind(env, x, x'))
                  env (live, live')
          in
            case ret
             of SOME v => (v, env)
              | NONE => (i64const 1, env)
            (* end case *)
          end

    (* generate LLVM code for the given expression *)
    fun genExp (env, CFG.EXP(ppt, e)) = let
          val blk = Env.block env
          in
            case e
             of CFG.LET(x, rhs, e) => let
                  val _ = LB.emitComment(blk, concat[
                        "LET ", CFGVar.toString x, " = ", CFGUtil.rhsToString rhs
                      ])
                  val (result, env) = genRHS (env, blk, ppt, rhs)
                  in
                    genExp (Env.bind(env, x, result), e)
                  end
              | CFG.IF(tst, vs, jmp1, jmp2) => raise Fail "YOUR CODE HERE"
              | CFG.TAIL_APPLY(appl as (ty, f, vs)) => raise Fail "YOUR CODE HERE"
              | CFG.GOTO jmp => LB.emitBr (blk, genJump (env, jmp))
              | CFG.RETURN v => LB.emitReturn (blk, SOME(cvtVal env v))
            (* end case *)
          end

    (* generate LLVM code for the right-hand-side of a let binding.  This
     * function returns a pair of the result and environment (the environment
     * is updated for calls, since the mapping for live variables changes.
     *)
    and genRHS (env, blk, ppt, rhs) = let
          val blk = Env.block env
          in
            case rhs
             of CFG.APPLY(appl as (ty, f, vs)) => raise Fail "YOUR CODE HERE"
              | CFG.CALL(cf, vs) => raise Fail "YOUR CODE HERE"
              | CFG.PRIM(p, vs) => (case (p, vs)
                   of (P.IntAdd, [a, b]) => raise Fail "YOUR CODE HERE"
                    | (P.IntSub, [a, b]) => raise Fail "YOUR CODE HERE"
                    | (P.IntMul, [a, b]) => raise Fail "YOUR CODE HERE"
                    | (P.IntDiv, [a, b]) => raise Fail "YOUR CODE HERE"
                    | (P.IntMod , [a, b]) => raise Fail "YOUR CODE HERE"
                    | (P.IntNeg, [a]) => raise Fail "YOUR CODE HERE"
                    | (P.StrSize, [a]) => raise Fail "YOUR CODE HERE"
                    | (P.StrSub, [a, b]) => raise Fail "YOUR CODE HERE"
                    | (P.RefAssign, [a, b]) => raise Fail "YOUR CODE HERE"
                    | (P.RefDeref, [a]) =>raise Fail "YOUR CODE HERE"
                    | _ => raise Fail("bogus primop " ^ P.toString p)
                  (* end case *))
              | CFG.ALLOC vs => raise Fail "YOUR CODE HERE"
              | CFG.SEL(i, v) => raise Fail "YOUR CODE HERE"
            (* end case *)
          end

    (* handle a jump to another fragment/block; return the target label *)
    and genJump (env, (lab, args)) = (case CFGLabel.kindOf lab
           of CFGLabel.LK_Local frag => let
                val fromBlk = Env.block env
                val args' = List.map (cvtVal env) args
                val blk' = Info.blockOf frag
                (* update phi nodes for the arguments *)
                fun addPhiDefs phis = ListPair.appEq
                      (fn (phi, arg) => LB.addPhiDef (phi, arg, fromBlk))
                        (phis, args')
                in
                  raise Fail "YOUR CODE HERE";
                  LB.labelOf blk'
                end
            | _ => raise Fail "expected local label"
          (* end case *))

    fun genFunc env func = let
          val entryFrag = CFGFunct.entryOf func
          (* analyze the function and create its LLVM counter part *)
          val func' = Info.analyze (Env.moduleOf env, func)
          val entryBlk = LF.entryOf func'
          (* initialize the environment with the mapping from CFG parameters to LLVM *)
          val params = CFGFrag.paramsOf entryFrag
          val params' = List.map LV.reg (LF.paramsOf func')
          val env = Env.beginBlock (env, entryBlk, params, params')
          in
            genExp (env, CFGFrag.bodyOf entryFrag)
          end

    fun gen (baseName, CFG.PROG functions) = let
          val module = LLVMModule.new ()
          val env = Env.new module
          in
            (* generate the extern decls for runtime functions *)
            MMLRuntime.declareRuntimeGlobals module;
            (* generate code for functions *)
            List.app (genFunc env) functions;
            (* output code to file *)
            LLVMModule.output (baseName ^ ".ll", module)
          end

  end
