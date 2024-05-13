(* check-cfg.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Invariant checker for the CFG IR.
 *)

structure CheckCFG : sig

  (* `check prog` checks the CFG program for the following invariants:
   *
   *    - every function that is referenced is defined
   *    - every fragment that is referenced is defined (in the same function as
   *      the reference).
   *    - fragment arities are correct
   *    - variables are bound before use and used in scope
   *    - runtime-system functions and primitive operations have the correct
   *      number of arguments.
   *
   * It reports error to the stdErr stream and will return `true` if there were
   * errors and `false` otherwise.
   *)
    val check : CFG.program -> bool

  end = struct

    structure L = CFGLabel
    structure LTbl = CFGLabel.Tbl
    structure V = CFGVar
    structure VSet = V.Set

  (* an environment for keeping track of global and per-function information *)
    datatype env = E of {
        fns : CFGFunct.t LTbl.hash_table,
        frags : CFGFrag.t LTbl.hash_table,
        vars : VSet.set ref
      }

    fun entryLab f = CFGFrag.labelOf (CFGFunct.entryOf f)

  (* record the functions of a program to define the initial environment *)
    fun recordFns funcs = let
          val fns = LTbl.mkTable (16, Fail "funct tbl")
          val frags = LTbl.mkTable (16, Fail "frag tbl")
          fun record f = LTbl.insert fns (entryLab f, f)
          in
            List.app record funcs;
            E{fns = fns, frags = frags, vars = ref VSet.empty}
          end

    fun funIsDefined (E{fns, ...}, lab) = LTbl.inDomain fns lab

  (* record the fragments for a function *)
    fun recordFrags (E{frags, ...}, entry, fragments) = let
          fun record frag = LTbl.insert frags (CFGFrag.labelOf frag, frag)
          in
            LTbl.clear frags;
            record entry;
            List.app record fragments
          end

    fun findFrag (E{frags, ...}, lab) = LTbl.find frags lab

    fun recordVar (E{vars, ...}, x) =
          VSet.member(!vars, x) orelse (vars := VSet.add(!vars, x); false)

    val val2s = CFGUtil.valueToString

  (* some context helpers *)
    fun fragCxt (CFG.FRAG{lab, params, ...}) = concat[
            "`fragment ", L.toString lab, " (",
            String.concatWithMap "," V.toString params, ") = ...`"
          ]
    fun letCxt x = concat["`let ", V.toString x, " = ...`"]
    fun appCxt (kind, f, vs) = concat[
            "`", kind, " ", val2s f, " (", String.concatWithMap "," val2s vs, ")`"
          ]
    fun primCxt (p, vs) = concat[
            "`", Prim.toString p, " (", String.concatWithMap "," val2s vs, ")`"
          ]
    fun callCxt (cf, vs) = concat[
            "`call ", Runtime.nameOf cf, " (", String.concatWithMap "," val2s vs, ")`"
          ]
    fun selectCxt (i, v) = concat["`", "#", Int.toString i, "(", val2s v, ")`"]
    fun ifCxt (tst, vs) = concat[
          "`if ", PrimCond.toString tst, " (",
          String.concatWithMap "," val2s vs, ")` ...`"
        ]
    fun thenCxt cxt = "then branch of " ^ cxt
    fun elseCxt cxt = "else branch of " ^ cxt

    fun prErr msg = TextIO.output(TextIO.stdErr, concat msg)

    fun error (cxt, msg) = prErr ("** [" :: cxt :: "]: " :: msg @ ["\n"])

  (* a flag for marking fragments that have already been visited *)
    local
      val {getFn, setFn} = CFGFrag.newFlag ()
    in
    val visited = getFn
    fun markVisited frag = setFn(frag, true)
    fun clearVisited frag = setFn(frag, false)
    end (* local *)

    fun check (CFG.PROG fns) = let
          val anyErrors = ref false
          fun err (cxt, msg) = (
                if not (!anyErrors)
                  then (
                    prErr ["***** Errors detected in CFG *****\n"];
                    anyErrors := true)
                  else ();
                error (cxt, msg))
        (* initialize the environment *)
          val env = recordFns fns
        (* check that a variable has not been previoously bound in the program. *)
          fun chkBinding cxt (x, bnd) = (
                if recordVar (env, x)
                  then error (cxt, ["multiple bindings for '", V.toString x, "'"])
                  else ();
                VSet.add(bnd, x))
          fun chkValue (bnd, cxt) (CFG.VAR x) = if not (VSet.member(bnd, x))
                then error (cxt, ["unbound use occurrence of `", V.toString x, "`"])
                else ()
            | chkValue (bnd, cxt) (CFG.CODE lab) = if not (funIsDefined (env, lab))
                then error (cxt, [
                    "reference to undefined function label `", L.toString lab, "`"
                  ])
                else ()
            | chkValue _ _ = ()
          fun chkValues (bnd, cxt, vs) = List.app (chkValue (bnd, cxt)) vs
        (* check a function *)
          fun chkFun (CFG.FUN{entry, frags, ...}) = let
                fun postChk frag = if visited frag
                      then clearVisited frag
                      else err (
                        concat["`fun ", L.nameOf(CFGFrag.labelOf entry), "...`"],
                        ["fragment '", L.toString(CFGFrag.labelOf frag), "' is unreachable"])
                in
                  recordFrags (env, entry, frags);
                  chkFrag entry;
                (* check for unvisited fragments and clear visited marks *)
                  List.app postChk frags;
                  clearVisited entry
                end
          and chkFrag (frag as CFG.FRAG{lab, params, body}) = (
                markVisited frag;
                chkExp (List.foldl (chkBinding (fragCxt frag)) VSet.empty params, body))
        (* check the body of a fragment *)
          and chkExp (bnd, CFG.EXP(_, e)) = (case e
                 of CFG.LET(x, rhs, e) => (
                      case rhs
                       of CFG.APPLY(_, fv, vs) => let
                            val cxt = appCxt ("apply ", fv, vs)
                            in
                              chkValue (bnd, cxt) fv;
                              chkValues (bnd, cxt, vs)
                            end
                        | CFG.CALL(cf, vs) => let
                            val cxt = callCxt (cf, vs)
                            in
                              if Runtime.arityOf cf <> length vs
                                then err(cxt, ["arity mismatch"])
                                else ();
                              chkValues (bnd, cxt, vs)
                            end
                        | CFG.PRIM(p, vs) => let
                            val cxt = primCxt (p, vs)
                            in
                              if Prim.arityOf p <> length vs
                                then err(cxt, ["arity mismatch"])
                                else ();
                              chkValues (bnd, cxt, vs)
                            end
                        | CFG.ALLOC vs => chkValues (bnd, "alloc", vs)
                        | CFG.SEL(i, v) => chkValue (bnd, selectCxt(i, v)) v
                      (* end case *);
                      chkExp (chkBinding "let binding" (x, bnd), e))
                  | CFG.IF(tst, vs, jmp1, jmp2) => let
                      val cxt = ifCxt (tst, vs)
                      in
                        if PrimCond.arityOf tst <> length vs
                          then err(cxt, ["arity mismatch"])
                          else ();
                        chkValues (bnd, cxt, vs);
                        chkJump (bnd, thenCxt cxt, jmp1);
                        chkJump (bnd, elseCxt cxt, jmp2)
                      end
                  | CFG.TAIL_APPLY(_, fv, vs) => (
                      chkValue (bnd, appCxt ("tail apply ", fv, vs)) fv;
                      chkValues (bnd, "tail apply ", vs))
                  | CFG.GOTO jmp => chkJump (bnd, "goto", jmp)
                  | CFG.RETURN v => chkValue (bnd, "return") v
                (* end case *))
        (* check a jump *)
          and chkJump (bnd, cxt, (lab, vs)) = (
                chkValues (bnd, cxt, vs);
                case findFrag (env, lab)
                 of SOME(frag as CFG.FRAG{params, ...}) => (
                      if List.length params <> List.length vs
                        then err (cxt, ["arity mismatch"])
                        else ();
                      if not (visited frag)
                        then chkFrag frag
                        else ())
                 | NONE => error (cxt, ["jump to unknown label '", L.toString lab, "'"])
                (* end case *))
          in
            !anyErrors
          end

  end
