(* contract.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * This file implements contraction (aka shrinking) for the SimpleAST IR.  The optimizations
 * that it supports are:
 *
 *      - constant folding of arithmetic expressions
 *      - constant folding of selection from known tuples
 *      - constant folding of conditional expressions on known values.
 *      - constant folding of cases on known values
 *      - dead-code elimination
 *      - contractive inlining
 *)

structure Contract : sig

    (* contract the program; we assume that the census counts for the program's
     * variables are accurate.
     *)
    val transform : SimpleAST.program -> SimpleAST.program

  end = struct

    structure C = Census
    structure S = SimpleAST
    structure P = Prim
    structure V = SimpleVar
    structure DC = SimpleDataCon
    structure VSet = SimpleVar.Set
    structure VMap = SimpleVar.Map

    (* interesting things that a variable might be bound to *)
    datatype bind
      = Tuple of S.value list                   (* bound to a SimpleAST tuple *)
      | Value of S.value                        (* bound to a SimpleAST value *)
      | ConApp of DC.t * S.value list           (* bound to a data constructor application *)
      | Func of S.var * S.var list * S.exp      (* bound to a single-use function *)
      | Other                                   (* other/unknown binding *)

    (* a mapping from variables to values *)
    type binding_env = bind VMap.map

    type context = {
        env : binding_env,
        outerFns : VSet.set
      }

    val emptyCxt : context = {env = VMap.empty, outerFns = VSet.empty}

    fun bindVar ({env, outerFns}, x, b) =
          {env = VMap.insert(env, x, b), outerFns = outerFns}

    fun insideFun ({env, outerFns}, f) = {env = env, outerFns = VSet.add(outerFns, f)}

    (* bind a variable to a value; if the value is a variable, then we adjust its
     * use count.
     *)
    fun bindVarToVal (x, S.V_VAR y, cxt) = (
          (* we are removing one use of y and replacing x with y everywhere *)
          V.addUse(y, V.useCntOf x - 1);
          bindVar (cxt, x, Value(S.V_VAR y)))
      | bindVarToVal (x, v, cxt) = bindVar (cxt, x, Value v)

    fun bindVarsToVals ([], [], env) = env
      | bindVarsToVals (x::xs, v::vs, env) =
          bindVarsToVals (xs, vs, bindVarToVal(x, v, env))
      | bindVarsToVals _ = raise Fail "bindVarsToVals: arity mismatch"

    (* resolve a variable to its binding *)
    fun resolveVar ({env, outerFns}, x) = (case VMap.find (env, x)
           of SOME v => v
            | NONE => Other
          (* end case *))

    (* resolve a value to its definition *)
    fun resolveVal cxt (S.V_VAR x) = resolveVar (cxt, x)
      | resolveVal _ v = Value v

    (* decrement use counts for values *)
    fun decVal (S.V_VAR x) = V.decUse x
      | decVal _ = ()

    (* narrow the representation of n to 63 bits (2's complement).
     * This behaves like a C-style cast to a signed integer type.
     *)
    local
      fun pow2 w = IntInf.<<(1, Word.fromInt w)
      val limit = pow2 63
      val mask = limit - 1
      val posLimit = pow2 62
    in
    fun sNarrow n = let
          val n = IntInf.andb(n, mask)
          in
            if n < posLimit then n else n - limit
          end
    end (* local *)

    (* constant arithmetic with modulo 2^62 wrapping *)
    fun iAdd (a, b) = sNarrow (a + b)
    fun iSub (a, b) = sNarrow (a - b)
    fun iMul (a, b) = sNarrow (a * b)
    fun iDiv (a, b) = sNarrow (IntInf.quot(a, b))
    fun iRem (a, b) = sNarrow (IntInf.rem(a, b))
    fun iNeg a = sNarrow (~a)

    (* string length as an IntInf.int *)
    fun size' s = IntInf.fromInt(size s)

    (* the result of contracting a RHS *)
    datatype result
      = RESULT of S.value               (* RHS reduced to a value *)
      | RENAME of context * S.rhs       (* updated binding environment plus renamed rhs *)

    (* rename values by replacing variables with the value that
     * they are bound to (if any)
     *)
    fun rn {env, outerFns} (v as S.V_VAR x) = (case VMap.find(env, x)
             of SOME(Value v) => v
              | _ => v
            (* end case *))
      | rn _ v = v

    (* delete code with a renaming substitution *)
    local
      fun mkSubst env x = (case VMap.find(env, x)
             of SOME(Value(S.V_VAR x')) => x'
              | _ => x
            (* end case *))
      fun deleteWithSubst deleteFn ({env, outerFns}, term) =
            deleteFn ({subst = mkSubst env, outerFns = outerFns}, term)
    in
    val deleteRHS = deleteWithSubst C.deleteRHS
    val deleteExp = deleteWithSubst C.deleteExp
    fun deleteRule {env, outerFns} =
          C.deleteRule {subst = mkSubst env, outerFns = outerFns}
    end

    (* constant folding for primitive operators *)
    fun contractPrim (cxt, oper, args) = let
          val args = List.map (rn cxt) args
          fun result v = (
                List.app decVal args;
                RESULT v)
          (* return the renamed RHS when there is no contraction *)
          fun rename () = RENAME(cxt, S.R_PRIM(oper, args))
          fun mkInt n = result(S.V_INT n)
          in
            case (oper, args)
             of (P.IntAdd, [S.V_INT a, S.V_INT b]) =>
                  mkInt (sNarrow (a + b))
              | (P.IntSub, [S.V_INT a, S.V_INT b]) =>
                  mkInt (sNarrow (a - b))
              | (P.IntMul, [S.V_INT a, S.V_INT b]) =>
                  mkInt (sNarrow (a * b))
              | (P.IntDiv, [S.V_INT _, S.V_INT 0]) =>
                  (* will cause a runtime error, so cannot contract *)
                  rename ()
              | (P.IntDiv, [S.V_INT a, S.V_INT b]) =>
                  (mkInt (sNarrow (IntInf.quot(a, b))))
              | (P.IntMod, [S.V_INT _, S.V_INT 0]) =>
                  (* will cause a runtime error, so cannot contract *)
                  rename ()
              | (P.IntMod, [S.V_INT a, S.V_INT b]) =>
                  mkInt (sNarrow (IntInf.rem(a, b)))
              | (P.IntNeg, [S.V_INT a]) => mkInt (sNarrow (~a))
              | (P.StrSize, [S.V_STR a]) => mkInt (IntInf.fromInt(size a))
              | (P.StrSub, [S.V_STR a, S.V_INT b]) => let
                  val n = size' a
                  in
                    if (0 <= b) andalso (b < n)
                      then mkInt(IntInf.fromInt(ord(String.sub(a, Int.fromLarge b))))
                      else rename ()
                  end
              | _ => rename ()
            (* end case *)
          end

    (* constant folding for primitive conditional tests *)
    fun contractCond (cxt, oper, args) = (
          case (oper, List.map (rn cxt) args)
           of (PrimCond.IntLt, [S.V_INT a, S.V_INT b]) => SOME(a < b)
            | (PrimCond.IntLte, [S.V_INT a, S.V_INT b]) => SOME(a <= b)
            | (PrimCond.IntEq, [S.V_INT a, S.V_INT b]) => SOME(a = b)
            | (PrimCond.IntNEq, [S.V_INT a, S.V_INT b]) => SOME(a <> b)
            | _ => NONE
          (* end case *))

    fun isPureRHS (S.R_EXP e) = false
      | isPureRHS (S.R_PRIM(p, _)) = Prim.isPure p
      | isPureRHS (S.R_CALL(cf, _)) = Runtime.isPure cf
      | isPureRHS _ = true

    fun transform (S.PROG(vArgs, e)) = let
          val anyChange = ref false
          fun change () = anyChange := true
          (* smart constructor for let-bindings that does on-the-fly let floating *)
          fun mkLet (x, S.E(_, S.E_LET(y, rhs, e1)), e2) = (
                change();
                S.mkLET(y, rhs, S.mkLET(x, S.R_EXP e1, e2)))
            | mkLet (x, S.E(_, S.E_FUN(retTy, f, xs, body, e1)), e2) = (
                change();
                S.mkFUN(retTy, f, xs, body, S.mkLET(x, S.R_EXP e1, e2)))
            | mkLet (x, e1, e2) = S.mkLET(x, S.R_EXP e1, e2)
          fun xformExp (cxt, S.E(ppt, e)) = (case e
                 of S.E_LET(x, S.R_EXP e1, e2) => (case xformExp (cxt, e1)
                       of S.E(_, S.E_RET v) => let
                          (* replace `x` with `v` *)
                            val cxt = bindVarToVal (x, v, cxt)
                            in
                              change();
                              xformExp (cxt, e2)
                            end
                        | e1' => mkLet (x, e1', xformExp (cxt, e2))
                      (* end case *))
                  | S.E_LET(x, rhs, e) => if (V.unused x andalso isPureRHS rhs)
                      then ( (* eliminate unused variable and its binding *)
                        change();
                        deleteRHS (cxt, rhs);
                        xformExp (cxt, e))
                      else (case xformRHS(cxt, x, rhs)
                         of RESULT v => (
                              change();
                              V.decUse x;
                              xformExp (bindVarToVal(x, v, cxt), e))
                          | RENAME(cxt', rhs') => S.mkLET(x, rhs', xformExp(cxt', e))
                        (* end case *))
                  | S.E_FUN(retTy, f, xs, body, e) =>
                      if (C.insideCntOf f = V.useCntOf f)
                        then (
                          (* the function is not referenced outside its body *)
                          deleteExp (cxt, body);
                          xformExp (cxt, e))
                      else if (V.useCntOf f = 1) andalso (C.appCntOf f = 1)
                        then (
                          change();
                          (* sanity check *)
                          if C.insideCntOf f > 0
                            then raise Fail "bad census counts for function"
                            else ();
                          (* this function can be contracted by inlining *)
                          xformExp (
                            bindVar (insideFun (cxt, f), f, Func(f, xs, body)),
                            e))
                        else xformFB (cxt, retTy, f, xs, body, e)
                  | S.E_APPLY(f, vs) => let
                      val vs' = List.map (rn cxt) vs
                      in
                        case resolveVar(cxt, f)
                         of Func(f, xs, body) => let
                              (* `f` is only applied here so we inline expand it *)
                              val cxt' = ListPair.foldl bindVarToVal cxt (xs, vs')
                              in
                                C.decApp f; V.decUse f;
                                xformExp (cxt', body)
                              end
                          | Value(S.V_VAR f') => S.mkAPPLY(f', vs')
                          | _ => S.mkAPPLY(f, vs')
                        (* end case *)
                      end
                  | S.E_IF(tst, vs, e1, e2) => let
                      val vs' = List.map (rn cxt) vs
                      in
                        case contractCond (cxt, tst, vs')
                         of SOME true => (deleteExp (cxt, e2); xformExp(cxt, e1))
                          | SOME false => (deleteExp (cxt, e1); xformExp(cxt, e2))
                          | NONE => S.mkIF(tst, vs', xformExp(cxt, e1), xformExp(cxt, e2))
                        (* end case *)
                      end
                  | S.E_CASE(v, rules) => let
                    (* build a case expression while preserving the program point
                     * (and associated case information).
                     *)
                      fun mkCase arg = S.E(ppt, S.E_CASE arg)
                    (* contract the action of a rule *)
                      fun xformRule (p, e) = (p, xformExp(cxt, e))
                    (* reduce a case when we know the argument is the data constructor `dc` *)
                      fun reduce (dc, rules) = let
                          (* return true if a case rule matches the constructor `dc` *)
                            fun match (p, _) = (case p
                                   of S.P_DCON(dc', _) => DC.same(dc, dc')
                                    | S.P_VAR _ => true
                                  (* end case *))
                            in
                              case List.partition match rules
                               of ([rule], others) => (
                                    List.app (deleteRule cxt) others;
                                    rule)
                                | ([rule, dflt], others) => (
                                    deleteRule cxt dflt;
                                    List.app (deleteRule cxt) others;
                                    rule)
                                | _ => raise Fail "impossible: bogus case"
                              (* end case *)
                            end
                      val bind = resolveVal cxt v
                      in
                      (* if the case argment is a constructor, then we can resolve
                       * the case at compile time.
                       *)
                        case bind
                         of Value(S.V_CON dc) => (
                              change();
                              decVal v;
                              case reduce (dc, rules)
                               of (S.P_VAR x, e) => xformExp(bindVar(cxt, x, bind), e)
                                | _ => raise Match (* compiler bug *)
                              (* end case *))
                          | ConApp(dc, vs') => (
                              change();
                              decVal v;
                              case reduce (dc, rules)
                               of (S.P_DCON(_, xs), e) =>
                                    xformExp(bindVarsToVals(xs, vs', cxt), e)
                                | (S.P_VAR x, e) =>
                                    xformExp(bindVar(cxt, x, bind), e)
                              (* end case *))
                          | Value v' => mkCase(v', List.map xformRule rules)
                          | _ => mkCase(v, List.map xformRule rules)
                        (* end case *)
                      end
                  | S.E_RET v => S.mkRET(rn cxt v)
                (* end case *))
          and xformRHS (cxt, lhs, rhs) = (case rhs
                 of S.R_EXP e => raise Fail "impossible"
                  | S.R_PRIM(p, vs) => contractPrim(cxt, p, List.map (rn cxt) vs)
                  | S.R_CALL(cf, vs) => RENAME(cxt, S.R_CALL(cf, List.map (rn cxt) vs))
                  | S.R_TUPLE vs => let
                      val vs' = List.map (rn cxt) vs
                      in
                        RENAME(bindVar(cxt, lhs, Tuple vs'), S.R_TUPLE vs')
                      end
                  | S.R_SELECT(i, x) => (case resolveVar(cxt, x)
                       of Tuple vs => (change(); V.decUse x; RESULT(List.nth(vs, i)))
                        | Value(S.V_VAR x') => RENAME(cxt, S.R_SELECT(i, x'))
                        | Other => RENAME(cxt, rhs)
                        | _ => raise Fail "bogus binding"
                      (* end case *))
                  | S.R_DCON(dc, vs) => let
                      val vs' = List.map (rn cxt) vs
                      in
                        RENAME(bindVar(cxt, lhs, ConApp(dc, vs')), S.R_DCON(dc, vs'))
                      end
                (* end case *))
          (* contraction for function bindinds when there is no inlining *)
          and xformFB (cxt, retTy, f, params, body, e) = let
                fun chkUnused () = if V.unused f
                      then (
                        change();
                        deleteExp (cxt, body);
                        true)
                      else false
                in
                  if chkUnused()
                    then xformExp (cxt, e)
                    else let
                      val e' = xformExp (cxt, e)
                      in
                        if chkUnused()
                          then e'
                          else S.mkFUN(retTy, f, params, xformExp(cxt, body), e')
                      end
                end
          (* iterate the contraction to a fixed point *)
          fun iterate e = let
                val e' = xformExp (emptyCxt, e)
                in
                  if !anyChange
                    then (anyChange := false; iterate e')
                    else e'
                end
          in
            S.PROG(vArgs, iterate e)
          end

  end
