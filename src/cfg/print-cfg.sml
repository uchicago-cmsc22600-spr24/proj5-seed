(* print-cfg.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * CFG printer that produces a "human readable" representation of
 * the program.
 *)

structure PrintCFG : sig

    val output : TextIO.outstream * CFG.program -> unit

  end = struct

    structure V = CFGVar
    structure L = CFGLabel

    val rhsToS = CFGUtil.rhsToString
    val valToS = CFGUtil.valueToString

    val valsToS = String.concatWithMap ", " valToS

    fun output (outS, CFG.PROG fns) = let
          fun pr s = TextIO.output(outS, s)
          fun prl s = pr(concat s)
          fun prIndent 0 = ()
            | prIndent n = (pr "   "; prIndent(n-1))
          fun prFun (CFG.FUN{retTy, entry as CFG.FRAG{lab, ...}, frags}) = (
                prl [PrimType.toString retTy, " ", L.toString lab, ": {\n"];
                prFrag  "fun " entry;
                List.app (prFrag "and ") frags;
                pr "}\n")
          and prFrag prefix (CFG.FRAG{lab, params, body}) = (
                prIndent 1;
                prl [
                    prefix, L.toString lab, " (",
                    String.concatWithMap ", " V.toString params,
                    ") {\n"
                  ];
                prExp body;
                pr "   }\n")
          and prExp (CFG.EXP(_, e)) = (
                prIndent 2;
                case e
                 of CFG.LET(x, rhs, e) => (
                      prl ["let ", V.toString x, " = ", rhsToS rhs, "\n"];
                      prExp e)
                  | CFG.IF(tst, vs, jmp1, jmp2) => (
                      prl ["if ", PrimCond.toString tst, " (", valsToS vs, ")\n"];
                      prIndent 3; pr "then "; prJmp jmp1;
                      prIndent 3; pr "else "; prJmp jmp2)
                  | CFG.TAIL_APPLY(_, f, vs) =>
                      prl ["apply ", valToS f, " (", valsToS vs, ")\n"]
                  | CFG.GOTO jmp => prJmp jmp
                  | CFG.RETURN v => prl["ret ", valToS v, "\n"]
                (* end case *))
          and prJmp (lab, vs) = prl [
                  "goto ", L.toString lab, " (", valsToS vs, ")\n"
                ]
          in
            List.app prFun fns
          end

  end
