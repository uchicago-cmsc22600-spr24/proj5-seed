(* case-split.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * The case splitting pass splits mixed case expressions into two sub-cases;
 * one for unboxed constructors and one for boxed constructors.  The census
 * counts are maintained and the new case expressions have correct case info.
 *)

structure CaseSplit : sig

    (* sets of live variables *)
    type live_vars = SimpleVar.Set.set

    type rule = SimpleAST.pat * SimpleAST.exp

    (* a representation of a sequence of rules that accounts for when we
     * need tests to match patterns.  The nodes are annotated with the
     * sets of variables that are live upon entry to the node.
     *)
    datatype decision_tree
      (* the default shared between unboxed and boxed sub-cases. *)
      = SharedDflt of live_vars
      (* a rule that will always match *)
      | Dflt of live_vars * rule
      (* a test; note that the pattern of the rule will always be a P_DCON
       * pattern.
       *)
      | Test of live_vars * rule * decision_tree

    (* return the live variables of a decision tree node *)
    val liveVarsOf : decision_tree -> SimpleVar.t list

    (* `split (info, rules)` analyzes the list of case rules and partitions
     * them into rules for unboxed constructors, rules for boxed constructors,
     * and an optional shared default rule.  The results of the call to `split`
     * will conform to one of the following situations:
     *
     *  ubRules  bxRules  default
     *  -------------------------
     *  SOME ub    NONE    NONE         -- when all constructors are unboxed
     *    NONE   SOME bx   NONE         -- when all constructors are boxed
     *  SOME ub  SOME bx   NONE         -- the unboxed and boxed constructors are
     *                                     disjointly covered
     *  SOME ub  SOME bx  SOME d        -- rule `d` covers both boxed and unboxed
     *                                     constructors.
     *
     * Note that there is one corner case, where we have a transparent
     * data-constructor function.  We classify such constructors as boxed,
     * even when their argument type is unboxed, since in that case they
     * would have to be the only constructor of their type.
     *)
    val split : CaseInfo.t * rule list -> {
            ubRules : decision_tree option,
            bxRules : decision_tree option,
            default : rule option
          }

  end = struct

    structure S = SimpleAST
    structure VSet = SimpleVar.Set
    structure DC = SimpleDataCon
    structure PTy = PrimType
    structure LV = LiveVarAnalysis

    type live_vars = SimpleVar.Set.set

    type rule = SimpleAST.pat * SimpleAST.exp

    datatype decision_tree
      = SharedDflt of live_vars
      | Dflt of live_vars * rule
      | Test of live_vars * rule * decision_tree

    fun liveVarsOf (SharedDflt lv) = VSet.toList lv
      | liveVarsOf (Dflt(lv, _)) = VSet.toList lv
      | liveVarsOf (Test(lv, _, _)) = VSet.toList lv

    (* `mkOptTree (rules, optDefault)` returns an optional decision tree that
     * represents the rules.
     *)
    fun mkOptTree ([], SOME r) = SOME(SharedDflt(LV.liveVarsForRule r))
      | mkOptTree ([], NONE) = NONE
      | mkOptTree (rules, optDflt) = let
          fun mkTree ([], SOME r) = let
                val lv = LV.liveVarsForRule r
                in
                  (lv, SharedDflt lv)
                end
            | mkTree ([r], NONE) = let
                val lv = LV.liveVarsForRule r
                in
                  (lv, Dflt(lv, r))
                end
            | mkTree (r::rs, dflt) = let
                val (lv, dtr) = mkTree (rs, dflt)
                val lv = VSet.union(lv, LV.liveVarsForRule r)
                in
                  (lv, Test(lv, r, dtr))
                end
          in
            SOME(#2 (mkTree (rules, optDflt)))
          end

    fun split (info : CaseInfo.t, rules) = let
          fun part ([], numUB, ub, numBx, bx) = (ub, bx, NONE)
            | part ([r as (S.P_VAR _, _)], numUB, ub, numBx, bx) =
                (* classify the default rule based on the coverage *)
                if (numUB = #nConsts info)
                  then (ub, r::bx, NONE)
                else if (numBx = #nFuns info)
                  then (r::ub, bx, NONE)
                  else (ub, bx, SOME r)
            | part ((r as (S.P_DCON(_, []), _))::rules, numUB, ub, numBx, bx) =
                part (rules, numUB+1, r::ub, numBx, bx)
            | part ((r as (S.P_DCON(_, _), _))::rules, numUB, ub, numBx, bx) =
                part (rules, numUB, ub, numBx+1, r::bx)
            | part _ = raise Fail "bogus case expression"
          val (ub, bx, dflt) = part (rules, 0, [], 0, [])
          in {
            ubRules = mkOptTree (List.rev ub, dflt),
            bxRules = mkOptTree (List.rev bx, dflt),
            default = dflt
          } end

  end
