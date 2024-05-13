(* dump-simple-ast.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Dump the SimpleAST data structure in S-Expression syntax.
 *)

structure DumpSimpleAST : sig

    (* `dumpToFile suffix (stem, simple)` dumps the SimpleAST to the
     * file `stem`.`suffix`
     *)
    val dumpToFile : string -> string * SimpleAST.program -> unit

  end = struct

    structure T = SimpleAST
    structure V = SimpleVar
    structure DC = SimpleDataCon
    structure Ty = PrimType

    datatype sexp = datatype SExp.value

    (* Generators for mapping data constructors and variables to unique IDs.
     * We do this in a left-to-right pre-order traversal of binding sites to
     * ensure a canonical representation.
     *)
    structure ConIdGen = IdGenFn (DC.Tbl)
    structure VarIdGen = IdGenFn (V.Tbl)

    type env = {
        idOfCon : DC.t -> sexp,
        idOfVar : V.t -> sexp
      }

    fun newEnv () : env = {
            idOfCon = ConIdGen.new(),
            idOfVar = VarIdGen.new()
          }

    (* walk the program and assign unique IDs for bound identifiers *)
    fun assignIds (T.PROG(vArgs, body)) = let
          val env = newEnv ()
          fun assignConId dc = ignore (#idOfCon env dc)
          fun assignVarId x = ignore (#idOfVar env x)
          fun doValue (T.V_VAR x) = assignVarId x (* is a nop for well-formed programs *)
            | doValue (T.V_CON dc) = assignConId dc
            | doValue _ = ()
          fun doExp (T.E(_, e)) = (case e
                 of T.E_LET(x, rhs, e) => (assignVarId x; doRHS rhs; doExp e)
                  | T.E_FUN(_, f, xs, body, e) => (
                      assignVarId f; List.app assignVarId xs; doExp body; doExp e)
                  | T.E_APPLY(f, vs) => (assignVarId f; List.app doValue vs)
                  | T.E_IF(tst, vs, e1, e2) => (List.app doValue vs; doExp e1; doExp e2)
                  | T.E_CASE(v, rules) => (doValue v; List.app doRule rules)
                  | T.E_RET v => doValue v
                (* end case *))
          and doRHS (T.R_EXP e) = doExp e
            | doRHS (T.R_PRIM(_, vs)) = List.app doValue vs
            | doRHS (T.R_CALL(_, vs)) = List.app doValue vs
            | doRHS (T.R_TUPLE vs) = List.app doValue vs
            | doRHS (T.R_SELECT(i, x)) = assignVarId x
            | doRHS (T.R_DCON(dc, vs)) = (assignConId dc; List.app doValue vs)
          and doRule (p, e) = (doPat p; doExp e)
          and doPat (T.P_DCON(dc, xs)) = (assignConId dc; List.app assignVarId xs)
            | doPat (T.P_VAR x) = assignVarId x
          in
            assignVarId vArgs;
            doExp body;
            env
          end

    (* helper functions to construct the S-Expression `(sym ...)` *)
    local
      fun mkNode name = let
            val sym = SYMBOL(Atom.atom name)
            in
              fn args => LIST(sym :: args)
            end
    in
    val mkProg = mkNode "PROG"
    val mkVarVal = mkNode "V_VAR"
    val mkConVal = mkNode "V_CON"
    val mkIntVal = mkNode "V_INT"
    val mkStrVal = mkNode "V_STR"
    val mkLetExp = mkNode "E_LET"
    val mkFunExp = mkNode "E_FUN"
    val mkAppExp = mkNode "E_APPLY"
    val mkIfExp = mkNode "E_IF"
    val mkCaseExp = mkNode "E_CASE"
    val mkRetExp = mkNode "E_RET"
    val mkPrimRHS = mkNode "R_PRIM"
    val mkCallRHS = mkNode "R_CALL"
    val mkTupleRHS = mkNode "R_TUPLE"
    val mkSelectRHS = mkNode "R_SELECT"
    val mkConAppRHS = mkNode "R_DCON"
    val mkConPat = mkNode "P_DCON"
    val mkVarPat = mkNode "P_VAR"
    end (* local *)

    fun list f xs = LIST(List.map f xs)

    (* convert basic values to an S-expression *)
    fun con2list (env : env) dc = [STRING(DC.nameOf dc), #idOfCon env dc]
    fun con2sexp (env : env) dc = LIST(con2list env dc)
    fun var2list (env : env) x = [STRING(V.nameOf x), #idOfVar env x]
    fun var2sexp (env : env) x = LIST(var2list env x)

    fun value2sexp (env : env) v = (case v
           of T.V_VAR x => mkVarVal (var2list env x)
            | T.V_CON dc => mkConVal (con2list env dc)
            | T.V_INT n => mkIntVal [INT n]
            | T.V_STR s => mkStrVal [STRING s]
          (* end case *))

    fun prog2sexp (env : env) (T.PROG(vArgs, e)) =
          mkProg [LIST[var2sexp env vArgs], exp2sexp env e]

    and exp2sexp (env : env) (T.E(_, e)) = (case e
           of T.E_LET(x, rhs, e) => mkLetExp [
                  LIST[var2sexp env x, rhs2sexp env rhs], exp2sexp env e
                ]
            | T.E_FUN(_, f, xs, e1, e2) => mkFunExp [
                  LIST[var2sexp env f, list (var2sexp env) xs, exp2sexp env e1],
                  exp2sexp env e2
                ]
            | T.E_APPLY(f, vs) => mkAppExp (var2sexp env f :: List.map (value2sexp env) vs)
            | T.E_IF(tst, vs, e1, e2) => mkIfExp [
                  LIST(STRING(PrimCond.toString tst) :: List.map (value2sexp env) vs),
                  exp2sexp env e1, exp2sexp env e2
                ]
            | T.E_CASE(v, rules) =>
                mkCaseExp [value2sexp env v, list (rule2sexp env) rules]
            | T.E_RET v => mkRetExp [value2sexp env v]
          (* end case *))

    and rhs2sexp (env : env) rhs = (case rhs
           of T.R_EXP e => exp2sexp env e  (* don't bother with a node for this case *)
            | T.R_PRIM(p, vs) =>
                mkPrimRHS (STRING(Prim.toString p) :: List.map (value2sexp env) vs)
            | T.R_CALL(cf, vs) =>
                mkCallRHS (STRING(Runtime.nameOf cf) :: List.map (value2sexp env) vs)
            | T.R_TUPLE vs => mkTupleRHS (List.map (value2sexp env) vs)
            | T.R_SELECT(i, x) => mkSelectRHS [INT(IntInf.fromInt i), var2sexp env x]
            | T.R_DCON(dc, vs) =>
                mkConAppRHS (con2sexp env dc :: List.map (value2sexp env) vs)
          (* end case *))

    and rule2sexp (env : env) (p, e) = LIST[pat2sexp env p, exp2sexp env e]

    and pat2sexp (env : env) p = (case p
           of T.P_DCON(dc, xs) =>
                mkConPat (con2sexp env dc :: List.map (var2sexp env) xs)
            | T.P_VAR x => mkVarPat (var2list env x)
          (* end case *))

    fun dumpToFile suffix (stem, prog) =
          DumpUtil.dump suffix (stem, prog2sexp (assignIds prog) prog)

  end
