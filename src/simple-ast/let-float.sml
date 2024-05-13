(* let-float.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * An efficient let floater for SimpleAST.  After running this pass,
 * there will be no expressions of the forms
 *
 *      let x = let y = ... in ...
 *
 *      let x = fun f (xs) = ... in ...
 *)

structure LetFloat : sig

    val transform : SimpleAST.program -> SimpleAST.program

  end = struct

    structure S = SimpleAST

    (* a continuation-passing-style let floater *)
    fun denest e = let
          fun doE (exp as S.E(ppt, e), k) = (case e
                 of S.E_LET(xs, S.R_EXP e1, e2) => let
                      fun k' e'' = S.mkLET(xs, S.R_EXP e'', doE(e2, k))
                      in
                        doE (e1, k')
                      end
                  | S.E_LET(xs, rhs, e) => S.E(ppt, S.E_LET(xs, rhs, doE(e, k)))
                  | S.E_FUN(retTy, f, xs, body, e) =>
                      S.E(ppt, S.E_FUN(retTy, f, xs, denest body, doE(e, k)))
                  | S.E_IF(tst, vs, e1, e2) =>
                      k (S.E(ppt, S.E_IF(tst, vs, denest' e1, denest' e2)))
                  | S.E_CASE(v, rules) => k (S.E(ppt, S.E_CASE(v, List.map doRule rules)))
                  | _ => k exp
                (* end case *))
          and doRule (p, e) = (p, denest' e)
          and denest' e = doE (e, fn e' => e')
          in
            denest' e
          end

    fun transform (S.PROG(vArgs, e)) = S.PROG(vArgs, denest e)

  end
