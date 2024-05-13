(* case-contract.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * A simple contraction of constant case expressions for when we do not run the
 * more general optimization passes.
 *)

structure CaseContract : sig

    (* reduce constant case expressions; after this pass we have the invariant that
     * the argument to any case expression will be a variable.
     *)
    val transform : SimpleAST.program -> SimpleAST.program

  end = struct

    structure S = SimpleAST
    structure VSet = SimpleVar.Set

    fun doExp (outerFns, exp) = let
          fun doExp' (exp as S.E(ppt, e)) = let
                fun mk e = S.E(ppt, e)
                in
                  case e
                   of S.E_LET(x, S.R_EXP e1, e2) =>
                        mk(S.E_LET(x, S.R_EXP(doExp' e1), doExp' e2))
                    | S.E_LET(x, rhs, e) => mk(S.E_LET(x, rhs, doExp' e))
                    | S.E_FUN(ty, f, xs, body, e) =>
                        mk(S.E_FUN(
                          ty, f, xs, doExp(VSet.add(outerFns, f), body),
                          doExp' e))
                    | S.E_IF(tst, args, e1, e2) =>
                        mk(S.E_IF(tst, args, doExp' e1, doExp' e2))
                    | S.E_CASE(S.V_CON dc, rules) => let
                        val deleteRule =
                              Census.deleteRule {subst=Fn.id, outerFns=outerFns}
                        fun match [] = raise Fail "unexpected incomplete case"
                          | match [(S.P_VAR x, e)] = if SimpleVar.useCntOf x > 0
                              then S.mkLET(x, S.R_EXP(S.mkRET(S.V_CON dc)), e)
                              else e
                          | match ((r as (S.P_DCON(dc', []), e))::rules') =
                              if SimpleDataCon.same(dc, dc')
                                then (List.app deleteRule rules'; e)
                                else (deleteRule r; match rules')
                          | match (r::rules') = (deleteRule r; match rules')
                        in
                          match rules
                        end
                    | S.E_CASE(S.V_VAR x, rules) =>
                        mk(S.E_CASE(S.V_VAR x, List.map (fn (p, e) => (p, doExp' e)) rules))
                    | S.E_CASE(_, rules) => raise Fail "invalid case expression"
                    | _ => exp
                  (* end case *)
                end
          in
            doExp' exp
          end

    and transform (S.PROG(vArgs, e)) = S.PROG(vArgs, doExp(VSet.empty, e))

  end

