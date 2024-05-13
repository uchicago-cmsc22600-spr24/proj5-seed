(* dump-free-vars.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Dump the computed free variables for the functions in the program.  The
 * is traversed in a left-to-right and identifiers are assigned unique IDs
 * based on that traversal order.
 *)

structure DumpFreeVars : sig

    (* `dumpToFile (stem, simple)` dumps the free-variable information to
     * file `stem`.fvs
     *)
    val dumpToFile : string * SimpleAST.program -> unit

  end = struct

    structure S = SimpleAST
    structure V = SimpleVar

    structure VarIdGen = IdGenFn (V.Tbl)

    datatype sexp = datatype SExp.value

    type env = {
        idOfVar : V.t -> sexp
      }

    fun newEnv () : env = {
            idOfVar = VarIdGen.new()
          }

    (* walk the program and assign unique IDs for bound identifiers *)
    fun assignIds (S.PROG(vArgs, body)) = let
          val env = newEnv ()
          fun assignVarId x = ignore (#idOfVar env x)
          fun doValue (S.V_VAR x) = assignVarId x (* is a nop for well-formed programs *)
            | doValue _ = ()
          fun doExp (S.E(_, e)) = (case e
                 of S.E_LET(x, rhs, e) => (assignVarId x; doRHS rhs; doExp e)
                  | S.E_FUN(_, f, xs, body, e) => (
                      assignVarId f; List.app assignVarId xs; doExp body; doExp e)
                  | S.E_APPLY(f, vs) => (assignVarId f; List.app doValue vs)
                  | S.E_IF(tst, vs, e1, e2) => (List.app doValue vs; doExp e1; doExp e2)
                  | S.E_CASE(v, rules) => (doValue v; List.app doRule rules)
                  | S.E_RET v => doValue v
                (* end case *))
          and doRHS (S.R_EXP e) = doExp e
            | doRHS (S.R_PRIM(_, vs)) = List.app doValue vs
            | doRHS (S.R_CALL(_, vs)) = List.app doValue vs
            | doRHS (S.R_TUPLE vs) = List.app doValue vs
            | doRHS (S.R_SELECT(_, x)) = assignVarId x
            | doRHS (S.R_DCON(_, vs)) = List.app doValue vs
          and doRule (p, e) = (doPat p; doExp e)
          and doPat (S.P_DCON(_, xs)) = List.app assignVarId xs
            | doPat (S.P_VAR x) = assignVarId x
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
    val mkFunExp = mkNode "E_FUN"
    end (* local *)

    fun var2sexp (env : env) x = LIST[STRING(V.nameOf x), #idOfVar env x]

    fun list f xs = LIST(List.map f xs)

    fun prog2sexp (env : env) (S.PROG(vArgs, e)) =
          mkProg (exp2list env e)

    and exp2sexp (env, e) = LIST(exp2list env e)

    and exp2list (env : env) (S.E(_, e)) = (case e
           of S.E_LET(_, S.R_EXP e1, e2) => exp2list env e1 @ exp2list env e2
            | S.E_LET(_, _, e) => exp2list env e
            | S.E_FUN(_, f, xs, e1, e2) => mkFunExp [
                  var2sexp env f,
                  list (var2sexp env) (FreeVarAnalysis.freeVarsOf f),
                  exp2sexp (env, e1)
                ] :: exp2list env e2
            | S.E_IF(tst, vs, e1, e2) => exp2list env e1 @ exp2list env e2
            | S.E_CASE(_, rules) =>
                List.foldr (fn (r, fns) => rule2sexp env r @ fns) [] rules
            | _ => []
          (* end case *))

    and rule2sexp (env : env) (p, e) = exp2list env e

    fun dumpToFile (stem, prog) =
          DumpUtil.dump "fvs" (stem, prog2sexp (assignIds prog) prog)

  end
