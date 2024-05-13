(* live-var-analysis.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * An analysis pass that computes the live variables at program points for the
 * first-order SimpleAST IR.  Since the fun-bound identifiers will have global
 * scope in the CFG IR, we filter them out of the live-variable sets.
 *)

structure LiveVarAnalysis : sig

    (* Analyze a first-order SimpleAST program to compute the live variables
     * at program points.
     *)
    val analyze : SimpleAST.program -> unit

    (* return the variables that are live upon entry to an expression. *)
    val liveVarsAt : SimpleAST.exp -> SimpleVar.Set.set

    (* return the live variables for a case rule *)
    val liveVarsForRule : SimpleAST.pat * SimpleAST.exp -> SimpleVar.Set.set

    (* return true if the function has a tail-recursive call to itself *)
    val isSelfTailRecursive : SimpleVar.t -> bool

  end = struct

    structure S = SimpleAST
    structure V = SimpleVar
    structure Set = V.Set

    (* program-point property to track live variables of expressions *)
    local
      val {getFn, setFn, ...} = ProgPt.newProp (fn _ => Set.empty)
    in
    fun liveVarsAt (S.E(ppt, _)) = getFn ppt
    val setLiveVars = setFn
    end (* local *)

    (* function property to mark self-tail-recursive functions. *)
    local
      val {getFn, setFn} = V.newFlag ()
    in
    val isSelfTailRecursive = getFn
    fun markSelfRecursive f = setFn(f, true)
    end (* local *)

    (* function property to mark variables that are the names of code *)
    local
      val {getFn, setFn} = V.newFlag ()
    in
    val isFunBound = getFn
    fun markFunBound f = setFn(f, true)
    end (* local *)

    (* compute live variables and record them at program points.  This analysis can
     * be done in one bottom-up pass over the body of a function.
     *
     * Note that we are not doing anything smart about loops that have free variables.
     * In ideal world, we would move the loading of the free variables into a loop
     * header block in the CFG, which would have to be accounted for by the live-variable
     * analysis.
     *)
    fun analyze (S.PROG(_, body)) = let
          fun add (live, x) = if isFunBound x then live else Set.add(live, x)
          fun lvOfVal (S.V_VAR x, live) = add(live, x)
            | lvOfVal (_, live) = live
          fun lvsOfVals (vs, live) = List.foldl lvOfVal live vs
          (* analyze a function body for live variables *)
          fun analFun (f, body) = let
                (* analyse an expression for variable liveness; `liveOut` is
                 * the set of live variables at the end of the expression (empty
                 * when the expression is in a tail position.
                 *)
                fun analExp (S.E(ppt, e), liveOut) = let
                      val live = (case e
                             of S.E_LET(x, rhs, e) =>
                                  analRHS (rhs, Set.subtract (analExp(e, liveOut), x))
                              | S.E_FUN(_, f, xs, body, e) => (
                                  markFunBound f;
                                  analFun (f, body);
                                  (* note that since `f` is fun-bound, it should not
                                   * be recorded as live in `e`, so we do not need to
                                   * remove it.
                                   *)
                                  analExp(e, liveOut))
                              | S.E_APPLY(g, vs) => (
                                  (* check for self-tail recursion *)
                                  if V.same(f, g) then markSelfRecursive f else ();
                                  add(lvsOfVals(vs, liveOut), g))
                              | S.E_IF(_, vs, e1, e2) =>
                                  lvsOfVals (vs,
                                    Set.union(analExp(e1, liveOut), analExp(e2, liveOut)))
                              | S.E_CASE(v, rules) => let
                                  fun analRule ((p, e), live) = let
                                        val lv = analExp(e, liveOut)
                                        val liveIn = (case p
                                               of S.P_DCON(_, xs) =>
                                                    Set.subtractList(lv, xs)
                                                | S.P_VAR x =>
                                                    Set.subtract(lv, x)
                                              (* end case *))
                                        in
                                          Set.union (liveIn, live)
                                        end
                                  in
                                    lvOfVal (v, List.foldl analRule Set.empty rules)
                                  end
                              | S.E_RET v => lvOfVal (v, liveOut)
                            (* end case *))
                      in
                        setLiveVars (ppt, live);
                        live
                      end
                and analRHS (rhs, liveOut) = (case rhs
                       of S.R_EXP e => analExp(e, liveOut)
                        | S.R_PRIM(_, vs) => lvsOfVals (vs, liveOut)
                        | S.R_CALL(_, vs) => lvsOfVals (vs, liveOut)
                        | S.R_TUPLE vs => lvsOfVals (vs, liveOut)
                        | S.R_SELECT(_, x) => add(liveOut, x)
                        | S.R_DCON(_, vs) => lvsOfVals (vs, liveOut)
                      (* end case *))
                and analRule ((p, e), live) = let
                      val lv = analExp e
                      val liveIn = (case p
                             of S.P_DCON(_, xs) => Set.subtractList(lv, xs)
                              | S.P_VAR x => Set.subtract(lv, x)
                            (* end case *))
                      in
                        Set.union (liveIn, live)
                      end
                in
                  ignore (analExp(body, Set.empty))
                end
          in
            analFun (V.progName, body)
          end

    fun liveVarsForRule (S.P_DCON(_, xs), e) = Set.subtractList(liveVarsAt e, xs)
      | liveVarsForRule (S.P_VAR x, e) = Set.subtract(liveVarsAt e, x)

  end
