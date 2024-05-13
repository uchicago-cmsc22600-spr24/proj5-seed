(* dump-cfg.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Dump the CFG data structure in S-Expression syntax.
 *)

structure DumpCFG : sig

  (* `dumpToFile (stem, cfg)` dumps the CFG to the file `stem`.cfg *)
    val dumpToFile : string * CFG.program -> unit

  end = struct

    structure T = CFG
    structure V = CFGVar
    structure L = CFGLabel
    structure Ty = PrimType

    datatype sexp = datatype SExp.value

  (* helper functions to construct the S-Expression `(sym ...)` *)
    local
      fun mkNode name = let
            val sym = SYMBOL(Atom.atom name)
            in
              fn args => LIST(sym :: args)
            end
    in
    val mkPROG = mkNode "PROG"
    val mkFUNCTION = mkNode "FUN"
    val mkENTRY = mkNode "ENTRY"
    val mkFRAG = mkNode "FRAG"
    val mkVAR = mkNode "VAR"
    val mkINT = mkNode "INT"
    val mkSTRING = mkNode "STRING"
    val mkCODE = mkNode "CODE"
    val mkLET = mkNode "LET"
    val mkIF = mkNode "IF"
    val mkTAIL_APPLY = mkNode "TAIL_APPLY"
    val mkGOTO = mkNode "GOTO"
    val mkRETURN = mkNode "RETURN"
    val mkAPPLY = mkNode "APPLY"
    val mkCALL = mkNode "CALL"
    val mkPRIM = mkNode "PRIM"
    val mkTUPLE = mkNode "ALLOC"
    val mkSEL = mkNode "SEL"
    end (* local *)

    fun list f xs = LIST(List.map f xs)

  (* convert basic values to an S-expression *)
    fun lab2sexp lab = STRING(L.toString lab)
    fun var2sexp x = STRING(V.toString x)

    fun value2sexp v = (case v
           of T.VAR x => mkVAR [var2sexp x]
            | T.INT n => mkINT [INT n]
            | T.STRING s => mkSTRING [STRING s]
            | T.CODE lab => mkCODE [lab2sexp lab]
          (* end case *))

    fun prog2sexp (T.PROG fns) = mkPROG (List.map fun2sexp fns)

    and fun2sexp (T.FUN{entry, frags, ...}) = let
          fun frag2list (T.FRAG{lab, params, body}) = [
                  lab2sexp lab, LIST(List.map var2sexp params), exp2sexp body
                ]
          in
            mkFUNCTION(mkENTRY(frag2list entry) :: List.map (mkFRAG o frag2list) frags)
          end

    and exp2sexp (T.EXP(_, e)) = (case e
           of T.LET(x, rhs, e) => mkLET [
                  var2sexp x, rhs2sexp rhs, exp2sexp e
                ]
            | T.IF(tst, vs, jmp1, jmp2) => mkIF [
                  LIST(STRING(PrimCond.toString tst) :: List.map value2sexp vs),
                  jump2sexp jmp1, jump2sexp jmp2
                ]
            | T.TAIL_APPLY(_, f, vs) =>
                mkTAIL_APPLY (value2sexp f :: List.map value2sexp vs)
            | T.GOTO jmp => jump2sexp jmp
            | T.RETURN v => mkRETURN [value2sexp v]
          (* end case *))

    and jump2sexp (lab, vs) =
          mkGOTO (lab2sexp lab :: List.map value2sexp vs)

    and rhs2sexp rhs = (case rhs
           of T.APPLY(_, f, vs) =>
                mkAPPLY (value2sexp f :: List.map value2sexp vs)
            | T.PRIM(p, vs) =>
                mkPRIM (STRING(Prim.toString p) :: List.map value2sexp vs)
            | T.CALL(cf, vs) =>
                mkCALL (STRING(Runtime.nameOf cf) :: List.map value2sexp vs)
            | T.ALLOC vs => mkTUPLE (List.map value2sexp vs)
            | T.SEL(i, v) => mkSEL [INT(IntInf.fromInt i), value2sexp v]
          (* end case *))

    fun dumpToFile (stem, prog) =
          DumpUtil.dump "cfg" (stem, prog2sexp prog)

  end
