(* closure.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Convert the higher-order SimpleAST IR to the first-order SimpleAST IR.
 *)

structure Closure : sig

    (* transform the higher-order SimpleAST program to a first-order
     * SimpleAST program by applying closure conversion.  We assume that
     * the free variable analysis has already been run on the program.
     *)
    val transform : SimpleAST.program -> SimpleAST.program

  end = struct

    structure S = SimpleAST
    structure SV = SimpleVar
    structure VMap = SV.Map
    structure VSet = SV.Set
    structure FVA = FreeVarAnalysis

    (********** Renaming environment **********)

    (* the environment tracks the renaming of free variables and the set of
     * fun-bound variables that are currently in scope.
     *)
    type env = {
        rn : SV.t VMap.map,
        fns : VSet.set
      }

    (* the empty environment *)
    val empty : env = {rn = VMap.empty, fns = VSet.empty}

    (* rename a variable *)
    fun rename (env : env, x) = (case VMap.find(#rn env, x)
           of SOME y => y
            | NONE => x
          (* end case *))

    (* insert a renaming for a free variable into the environment *)
    fun insertFV (x, y, {rn, fns}) : env = {rn = VMap.insert(rn, x, y), fns=fns}

    (* record a that `f` is bound *)
    fun bindFun ({rn, fns}, f) : env = {rn = rn, fns = VSet.add(fns, f)}

    (* `isFunBound (env, f)` returns true if `f` is a fun-bound variable in scope *)
    fun isFunBound ({rn, fns}, f) = VSet.member(fns, f)

    (* return true if `f` is a known function (we assume that it is fun-bound) *)
    fun isKnown f = (SV.useCntOf f = Census.appCntOf f)

    (* a property for mapping functions to the name of their first-order counterpart *)
    val {getFn=firstOrderFunOf, ...} =
          SV.newProp (fn f => SV.new(SV.nameOf f ^ "_code", PrimType.CODE))

    (********** Conversion **********)

    (* convert a SimpleAST variable; the result will be a value *)
    fun cvtVar env x = S.V_VAR(rename (env, x))

    (* convert a SimpleAST value *)
    fun cvtVal env v = (case v
           of S.V_VAR x => S.V_VAR(rename (env, x))
            | v => v
          (* end case *))

    (* convert a list of SimpleAST values *)
    fun cvtVals (env, vs) = List.map (cvtVal env) vs

    fun cvtExp (env, S.E(ppt, e)) = let
          fun mk e = S.E(ppt, e)
          in
            case e
             of S.E_LET(x, rhs, e) => mk (S.E_LET(x, cvtRHS(env, rhs), cvtExp(env, e)))
              | S.E_FUN(retTy, f, xs, e1, e2) => let
                  val env' = bindFun(env, f)
                  val e2' = cvtExp (env', e2)
                  val fCode = firstOrderFunOf f
                  val fvs = FVA.freeVarsOf f
                  fun bindFVS (f', i, e) = let
                        fun bind (_, [], env) = cvtExp (env, e)
                          | bind (i, y::ys, env) = let
                              val y' = SV.copy y
                              in
                                S.mkSELECT(y', i, f',
                                  bind (i+1, ys, insertFV(y, y', env)))
                              end
                        in
                          bind (i, fvs, insertFV(f, f', env'))
                        end
                  in
                    if not (isKnown f)
                      (* the general case *)
                      then let
                        val f' = SV.copy f
                        in
                          mk (S.E_FUN(retTy, fCode, f'::xs, bindFVS (f', 1, e1),
                            S.mkTUPLE(f, S.V_VAR fCode :: List.map (cvtVar env) fvs,
                              e2')))
                        end
                    else if List.null fvs
                      (* known function with empty environment *)
                      then mk (S.E_FUN(retTy, fCode, xs, cvtExp(env', e1), e2'))
                      (* known function with non-empty environment *)
                      else let
                        val f' = SV.copy f
                        in
                          mk (S.E_FUN(retTy, fCode, f'::xs, bindFVS (f', 0, e1),
                            S.mkTUPLE(f, List.map (cvtVar env) fvs,
                              e2')))
                        end
                  end
              | S.E_APPLY(f, vs) => let
                  val f' = rename (env, f)
                  val vs' = cvtVals(env, vs)
                  in
                    if isFunBound (env, f)
                      (* `f` is in scope of its binding to a function *)
                      then if isKnown f andalso FVA.hasNoFreeVars f
                        (* `f` is a known function with an empty environment *)
                        then mk (S.E_APPLY(firstOrderFunOf f, vs'))
                        else mk (S.E_APPLY(firstOrderFunOf f, S.V_VAR f' :: vs'))
                      (* general case *)
                      else let
                        val f' = rename (env, f)
                        val cp = SV.new("cp", PrimType.CODE)
                        in
                          S.mkSELECT(cp, 0, f',
                          mk (S.E_APPLY(cp, S.V_VAR f' :: vs')))
                        end
                  end
              | S.E_IF(tst, vs, e1, e2) =>
                  mk (S.E_IF(tst, cvtVals(env, vs), cvtExp(env, e1), cvtExp(env, e2)))
              | S.E_CASE(v, rules) =>
                  mk (S.E_CASE(cvtVal env v, List.map (cvtRule env) rules))
              | S.E_RET v => mk (S.E_RET(cvtVal env v))
            (* end case *)
          end

    and cvtRHS (env, rhs) = (case rhs
           of S.R_EXP e => S.R_EXP(cvtExp(env, e))
            | S.R_PRIM(p, vs) => S.R_PRIM(p, cvtVals(env, vs))
            | S.R_CALL(cf, vs) => S.R_CALL(cf, cvtVals(env, vs))
            | S.R_TUPLE vs => S.R_TUPLE(cvtVals(env, vs))
            | S.R_SELECT(i, x) => S.R_SELECT(i, rename(env, x))
            | S.R_DCON(dc, vs) => S.R_DCON(dc, cvtVals(env, vs))
          (* end case *))

    and cvtRule env (p, e) = (p, cvtExp(env, e))

    (* run the analysis and closure conversion on the program *)
    fun transform (S.PROG(vArgs, body)) = S.PROG(vArgs, cvtExp(empty, body))

  end
