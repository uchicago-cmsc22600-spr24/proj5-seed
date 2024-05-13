(* print-simple.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * SimpleAST printer that produces a "human readable" representation of
 * the program.
 *)

structure PrintSimple : sig

    val output : TextIO.outstream * SimpleAST.program -> unit

  end = struct

    structure S = SimpleAST
    structure SV = SimpleVar
    structure DC = SimpleDataCon
    structure P = Prim
    structure PC = PrimCond

    fun valToS (S.V_VAR x) = SV.toString x
      | valToS (S.V_CON dc) = DC.nameOf dc
      | valToS (S.V_INT n) = if (n < 0)
          then "-" ^ IntInf.toString(~n)
          else IntInf.toString n
      | valToS (S.V_STR s) = concat["\"", String.toString s, "\""]

    fun valsToS vs = String.concatWithMap "," valToS vs

    fun varsToS xs = String.concatWithMap "," SV.toString xs

    fun isBindExp (S.E(_, S.E_LET _)) = true
      | isBindExp (S.E(_, S.E_FUN _)) = true
      | isBindExp _ = false

    fun isBigExp (S.E(_, S.E_LET _)) = true
      | isBigExp (S.E(_, S.E_FUN _)) = true
      | isBigExp (S.E(_, S.E_IF _)) = true
      | isBigExp (S.E(_, S.E_CASE _)) = true
      | isBigExp _ = false

    fun output (outS, S.PROG(vArgs, body)) = let
          fun pr s = TextIO.output(outS, s)
          fun prl s = pr(concat s)
          fun prIndent 0 = ()
            | prIndent n = (pr "   "; prIndent(n-1))
          (* print an expression with leading indentation and a trailing newline *)
          fun prExp (n, e) = (prIndent n; prExp' (n, e); pr "\n")
          (* print an expression without leading indentation or trailing newline *)
          and prExp' (n, S.E(_, e)) = (case e
                 of S.E_LET(x, rhs, e) => let
                      val _ = prl ["let ", SV.toString x, " ="]
                      val nl = prRHS (n, rhs)
                      in
                        prScope(nl, n, e)
                      end
                  | S.E_FUN(_, f, xs, e1, e2) => (
                      prl ["fun ", SV.toString f, " (", varsToS xs, ") ="];
                      if isBigExp e1
                        then (pr "\n"; prExp(n+1, e1))
                        else (pr " "; prExp' (n, e1); pr "\n");
                      prScope (isBigExp e1, n, e2))
                  | S.E_APPLY(f, vs) =>
                      prl [SV.toString f, " (", valsToS vs, ")"]
                  | S.E_IF(tst, vs, e1, e2) => (
                      prl ["if ", PC.toString tst, "(", valsToS vs, ") then\n"];
                      prExp (n+1, e1);
                      prIndent n; pr "else\n";
                      prIndent (n+1); prExp' (n+1, e2))
                  | S.E_CASE(v, rules) => let
                      fun prRule (p, e) = (
                            prIndent n; pr "| ";
                            case p
                             of S.P_VAR x => pr (SV.toString x)
                              | S.P_DCON(dc, []) => pr (DC.nameOf dc)
                              | S.P_DCON(dc, xs) => prl [
                                    DC.nameOf dc, "(", varsToS xs, ")"
                                  ]
                            (* end case *);
                            pr " =>";
                            if isBigExp e
                              then (pr "\n"; prExp(n+1, e))
                              else (pr " "; prExp'(n, e); pr "\n"))
                      in
                        prl ["case ", valToS v, " of\n"];
                        List.app prRule rules;
                        prIndent n; pr "end"
                      end
                  | S.E_RET v => prl["ret ", valToS v]
                (* end case *))
          and prScope (nl, n, e) =
                if isBindExp e
                  then (
                    if nl then (prIndent n; pr "in\n") else pr " in\n";
                    prIndent n;
                    prExp'(n, e))
                else if isBigExp e
                  then if nl
                    then (prIndent n; pr "in\n"; prIndent(n+1); prExp'(n+1, e))
                    else (pr " in\n"; prIndent(n+1); prExp'(n+1, e))
                else if nl
                  then (prIndent n; pr "in "; prExp'(n, e))
                  else (pr " in\n"; prIndent(n+1); prExp'(n, e))
          (* print a RHS followed by a newline *)
          and prRHS (n, rhs) = (case rhs
                 of S.R_EXP e => if isBigExp e
                      then (pr "\n"; prExp(n+1, e); true)
                      else (pr " "; prExp' (n, e); false)
                  | S.R_PRIM(p, vs) => (
                      prl[" ", Prim.toString p, "(", valsToS vs, ")"];
                      false)
                  | S.R_CALL(cf, vs) => (
                      prl[" call ", Runtime.nameOf cf, "(", valsToS vs, ")"];
                      false)
                  | S.R_TUPLE vs => (
                      prl[" [", valsToS vs, "]"];
                      false)
                  | S.R_SELECT(i, v) => (
                      prl[" #", Int.toString i, "(", SV.toString v, ")"];
                      false)
                  | S.R_DCON(dc, vs) => (
                      prl[" ", DC.toString dc, "(", valsToS vs, ")"];
                      false)
                (* end case *))
          in
            prl ["program (", SV.toString vArgs, ") =\n"];
            prExp (1, body)
          end

  end
