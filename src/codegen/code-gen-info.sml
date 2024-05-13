(* code-gen-info.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Support for various bits of useful info to support code generation.
 *)

structure CodeGenInfo : sig

    (* analyze a function in preparation for generating code for it.  This
     * process creates the LLVM function, allocates blocks for the
     * fragments, and identifies join points.
     *)
    val analyze : LLVMModule.t * CFGFunct.t -> LLVMFunc.t

    (* return the LLVM global for the given function label *)
    val funcGlobal : CFGLabel.t -> LLVMGlobal.t

    (* is a fragment a join fragment? *)
    val isJoin : CFGFrag.t -> bool

    (* get the block corresponding to a fragment *)
    val blockOf : CFGFrag.t -> LLVMBlock.t

    (* get the list of CFG variables that are live coming into the program point *)
    val getLive : ProgPt.t -> CFGVar.t list

  end = struct

    structure LTbl = CFGLabel.Tbl
    structure LB = LLVMBlock
    structure LG = LLVMGlobal
    structure Set = CFGVar.Set

    (* return the global for the given function label *)
    fun funcGlobal lab = (case CFGLabel.kindOf lab
           of CFGLabel.LK_Entry func => let
                val (retTy, argTys) = CFGFunct.typeOf func
                in
                  LG.new (
                    CFGFunct.nameOf func,
                    LLVMType.Proto(Util.cvtType retTy, List.map Util.cvtType argTys))
                end
            | _ => raise Fail(concat[
                  "funcGlobal: '", CFGLabel.toString lab, "' is not an entry label"
                ])
          (* end case *))

    (* property to associate an LLVM block with a fragment *)
    val {getFn = (blockOf : CFGFrag.t -> LB.t), setFn = setBlock, ...} =
          CFGFrag.newProp (fn frag => raise Fail("no block for " ^ CFGFrag.nameOf frag))

    (* a flag for marking join fragments *)
    local
      val {getFn, setFn} = CFGFrag.newFlag ()
    in
    val isJoin = getFn
    fun markJoin frag = setFn(frag, true)
    end (* local *)

    (* property to record the live variables at a program point. *)
    local
      val {getFn, setFn, ...} = ProgPt.newProp (fn _ => Set.empty)
    in
    val setLive = setFn
    fun getLive ppt = Set.toList(getFn ppt)
    end (* local *)

    fun analyze (module, func) = let
          (* table mapping labels to in-degree counts *)
          val labTbl = LTbl.mkTable (8, Fail "label table")
          fun inDegree lab = (case LTbl.find labTbl lab
                 of NONE => 0
                  | SOME n => n
                (* end case *))
          fun incInDegree lab = (case LTbl.find labTbl lab
                 of NONE => LTbl.insert labTbl (lab, 1)
                  | SOME n => LTbl.insert labTbl (lab, n + 1)
                (* end case *))
          (* record the out-going edges of a fragment body *)
          fun doExp (CFG.EXP(_, rep)) = (case rep
                 of CFG.LET(_, _, e) => doExp e
                  | CFG.IF(_, _, jmp1, jmp2) => (doJump jmp1; doJump jmp2)
                  | CFG.GOTO jmp => doJump jmp
                  | _ => ()
                (* end case *))
          and doJump (lab, _) = incInDegree lab
          (* create the LLVM function *)
          val lFunc = LLVMFunc.new module {
                  tailCC = true, (* not true for the entry, but fixed in the LLVM library *)
                  retTy = Util.cvtType(CFGFunct.returnTypeOf func),
                  name = CFGFunct.nameOf func,
                  params = Util.getParamRegs (CFGFunct.entryOf func)
                }
          (* process a non-entry fragment *)
          fun doFrag frag = let
                val blk = LLVMFunc.newBlock (lFunc, CFGFrag.nameOf frag)
                in
                  setBlock (frag, blk);
                  if inDegree(CFGFrag.labelOf frag) > 1
                    then markJoin frag
                    else ()
                end
          (* compute and record liveness information for a fragment *)
          fun liveness frag = raise Fail "YOUR CODE HERE"
          val frags = CFGFunct.entryOf func :: CFGFunct.fragmentsOf func
          in
            (* the first pass over the fragments determines which ones are joins *)
            List.app (doExp o CFGFrag.bodyOf) frags;
            (* map the entry fragment to the entry block of the function *)
            setBlock (CFGFunct.entryOf func, LLVMFunc.entryOf lFunc);
            (* the second pass allocates LLVM blocks and marks join fragments;
             * we skip the entry fragment, since its block is alread allocated
             *)
            List.app doFrag (CFGFunct.fragmentsOf func);
            (* do liveness analysis *)
            List.app liveness frags;
            (* return the LLVM function *)
            lFunc
          end

  end
