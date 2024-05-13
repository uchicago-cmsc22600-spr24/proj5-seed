(* simplify.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *)

structure Simplify : sig

    (* translate the AST IR to SimpleAST; in addition to converting the IR, we
     * also encapsulate the whole program in a function parameterized by the
     * `arguments` variable.
     *)
    val translate : AST.program -> SimpleAST.program

  end = struct

    structure S = SimpleAST
    structure SDCon = SimpleDataCon
    structure SVar = SimpleVar
    structure PTy = PrimType
    structure VMap = Var.Map
    structure B = SimpleBasis

    (* simplification environment; maps AST variables to SimpleAST variables. *)
    datatype env = Env of S.var VMap.map

    val emptyEnv = Env VMap.empty

    (* add a variable binding to the environment *)
    fun bindVar (Env vMap, x, x') = Env(VMap.insert(vMap, x, x'))

    (* look up a variable in the environment *)
    fun lookupVar (Env vMap, x) = (case VMap.find(vMap, x)
           of SOME x' => x'
            | NONE => raise Fail("unbound variable " ^ Var.toString x)
          (* end case *))

    fun repOfExp (AST.E(e, _)) = e
    fun typeOfExp (AST.E(_, ty)) = Type.prune ty

    val newTmpVar = SVar.fresh "_t"

    (* gather the arguments of a possibly curried application *)
    fun gatherArgs (AST.E(AST.AppExp(e1, e2), _), args) =
          gatherArgs (e1, e2::args)
      | gatherArgs (e, args) = (e, args)

    fun simplify (AST.Prog dcls) = let
          (* create the initial type representation environment *)
          val repInfo = SimplifyType.initial()
          (* analyse the type declarations *)
          val _ = let
                fun analTys (AST.TypeDcl tyc) = (SimplifyType.analyze repInfo) tyc
                  | analTys _ = ()
                in
                  List.app analTys dcls
                end
          (* convert from AST types to primitive types *)
          val cvtTy = SimplifyType.cvtTy repInfo
          val cvtScm = SimplifyType.cvtScheme repInfo
          (* convert from an AST data constructor to a SimpleAST constructor *)
          val cvtDCon = SimplifyType.conRepOf repInfo
          (* convert an AST variable to a SimpleAST variable and add the binding
           * to the environment
           *)
          fun cvtVar (env, x) = let
                val x' = SVar.new (Var.nameOf x, cvtScm(Var.typeOf x))
                in
                  (x', bindVar(env, x, x'))
                end
          (* convert expressions to values (the 𝒱⟦⟧ translation function) *)
          fun expToValue (env, exp, k : S.value -> S.exp) : S.exp = let
                (* fallback case for complicated expressions *)
                fun cvtExp () = let
                      val x' = newTmpVar (cvtTy (typeOfExp exp))
                      in
                        expToRHS (env, exp, fn rhs => S.mkLET(x', rhs, k (S.V_VAR x')))
                      end
                in
                  case repOfExp exp
                   of AST.VarExp x => k (S.V_VAR(lookupVar(env, x)))
                    | AST.ConExp dc => if DataCon.isNullary dc
                        then k (S.V_CON(cvtDCon dc))
                        else raise Fail "unexpected constructor function"
                    | AST.IntExp n => k (S.V_INT n)
                    | AST.StrExp s => k (S.V_STR s)
                    | _ => cvtExp ()
                  (* end case *)
                end
          (* convert expressions to variables *)
          and expToVar (env, e, k : SVar.t -> S.exp) =
                expToValue (env, e, fn (S.V_VAR x) => k x | _ => raise Match)
          (* convert a primitive operator to a right-hand-side expression; this
           * conversion includes adding error checking when necessary.
           *)
          and primToRHS (p, args, k) = let
                val resTy = Prim.resultTypeOf p
                (* build an expression that applies the give test operator to
                 * (v1, v2) and fails with the given message if the test is false.
                 *)
                fun mkChk (tst, v1, v2, msg) = let
                      val res = newTmpVar resTy
                      val dummy = newTmpVar resTy
                      in
                        S.mkIF(tst, [v1, v2],
                          S.mkLET(res, S.R_PRIM(p, args), S.mkRET(S.V_VAR res)),
                          S.mkLET(dummy, S.R_CALL(Runtime.funFail, [S.V_STR msg]),
                            S.mkRET(S.V_VAR dummy)))
                      end
                in
                  case (p, args)
                   of (Prim.IntDiv, [a, b]) =>
                        k (S.R_EXP(mkChk (PrimCond.IntNEq, b, S.V_INT 0, "Divide by zero")))
                    | (Prim.IntMod, [a, b]) =>
                        k (S.R_EXP(mkChk (PrimCond.IntNEq, b, S.V_INT 0, "Remainder by zero")))
                    | (Prim.StrSub, [a, b]) => let
                        val len = newTmpVar PTy.intTy
                        val chk = mkChk (PrimCond.UIntLt, b, S.V_VAR len, "Subscript out of bounds")
                        in
                          k (S.R_EXP(S.mkLET(len, S.R_PRIM(Prim.StrSize, [a]), chk)))
                        end
                    | _ => k (S.R_PRIM(p, args))
                  (* end case *)
                end
          (* convert expressions to right-hand-sides (the ℛ⟦⟧ translation function) *)
          and expToRHS (env, exp, k : S.rhs -> S.exp) : S.exp = (case repOfExp exp
                 of AST.AppExp(e1, e2) => let
                      val (fExp, args) = gatherArgs (e1, [e2])
                      (* fall-back function for non-primitive/non-constructor
                       * applications
                       *)
                      fun expRHS () = appToExp (env, fExp, args, fn e => k(S.R_EXP e))
                      fun trFnApp f = let
                            fun argsToVals (arity, k) = argToValues (env, arity, e2, k)
                            in
                              case B.lookup f
                               of B.PrimOp p =>
                                    argsToVals (Prim.arityOf p,
                                      fn vs => primToRHS(p, vs, k))
                                | B.CondOp tst => expRHS ()
                                | B.RTFun cf =>
                                    argsToVals (Runtime.arityOf cf,
                                      fn vs => k (S.R_CALL(cf, vs)))
                                | B.UserVar => expRHS ()
                              (* end case *)
                            end
                      in
                        case repOfExp fExp
                         of AST.VarExp f => trFnApp f
                          | AST.ConExp dc => conAppToRHS (env, dc, args, k)
                          | _ => expRHS ()
                        (* end case *)
                      end
                  | AST.TupleExp exps => let
                      fun e2v ([], vs) = k (S.R_TUPLE(List.rev vs))
                        | e2v (e::es, vs) = expToValue (env, e, fn v => e2v (es, v::vs))
                      in
                        e2v (exps, [])
                      end
                  | _ => k (S.R_EXP(expToExp (env, exp)))
                (* end case *))
          and conAppToRHS (env, dc, args, k : S.rhs -> S.exp) : S.exp = let
                (* convert arguments to values and then build the DCON rhs.
                 * Note that constructor applications are fully saturated
                 *)
                fun applyToArgs ([], args') =
                      k (S.R_DCON(cvtDCon dc, List.rev args'))
                  | applyToArgs (arg::args, args') =
                      expToValue (env, arg, fn arg' =>
                        applyToArgs (args, arg'::args'))
                in
                  applyToArgs (args, [])
                end
          (* convert a (possibly curried) application to an expression *)
          and appToExp (env, fExp, args, k : S.exp -> S.exp) : S.exp = let
                fun applyToArgs (f, _, []) = k (S.mkRET(S.V_VAR f))
                  | applyToArgs (f, _, [arg]) =
                      expToValue (env, arg, fn arg' => k (S.mkAPPLY(f, [arg'])))
                  | applyToArgs (f, ty, arg::args) = let
                      val (pResTy, resTy) = (case Type.asFunTy ty
                             of SOME(_, ty2) => let
				  val ty = Type.prune ty2
				  in
				    (cvtTy ty, ty)
				  end
                              | _ => raise Fail "expected function type"
                            (* end case *))
                      val t = newTmpVar pResTy
                      in
                        expToValue (env, arg, fn arg' =>
                          S.mkLET(t, S.R_EXP(S.mkAPPLY(f, [arg'])),
                            applyToArgs (t, resTy, args)))
                      end
                in
                  expToVar (env, fExp, fn f => applyToArgs(f, typeOfExp fExp, args))
                end
          (* flatten and simplify the arguments to a basis primitive
           * with the given arity
           *)
          and argToValues (env, 1, e, k) = expToValue (env, e, fn v => k [v])
            | argToValues (env, _, AST.E(AST.TupleExp exps, _), k) = let
                fun es2vs ([], vs) = k (List.rev vs)
                  | es2vs (e::es, vs) = expToValue (env, e, fn v => es2vs (es, v::vs))
                in
                  es2vs (exps, [])
                end
            | argToValues _ = raise Fail "unflattened arguments to primitive"
          (* convert an expression to a conditional test *)
          and condToExp (env, exp, tExp, fExp) = let
                (* evaluate the expression to a Bool value and then test it for `True` *)
                fun mkBoolTest () =
                      expToValue (env, exp, fn v => let
                        val caseExp = S.mkCASE(v, [
                                (S.P_DCON(B.conTrue, []), expToExp (env, tExp)),
                                (S.P_DCON(B.conFalse, []), expToExp (env, fExp))
                              ])
                        in
                          (* need to attach case info for later phases *)
                          CaseInfo.set (caseExp, {
                              ty = PTy.intTy, argKind = PTy.Unboxed,
                              nConsts = 2, nFuns = 0
                            });
                          caseExp
                        end)
                in
                  case repOfExp exp
                   of AST.AppExp(AST.E(AST.VarExp p, _), AST.E(AST.TupleExp[e1, e2], _)) => (
                      case B.lookup p
                       of B.CondOp tst =>
                            expToValue (env, e1, fn v1 =>
                              expToValue (env, e2, fn v2 =>
                                S.mkIF(tst, [v1, v2],
                                  expToExp (env, tExp),
                                  expToExp (env, fExp))))
                        | _ => mkBoolTest ()
                      (* end case *))
                    | _ => mkBoolTest ()
                  (* end case *)
                end
          (* convert expressions to expressions (the ℰ⟦⟧ translation function) *)
          and expToExp (env, exp) = (case repOfExp exp
                 of AST.IfExp(e1, e2, e3) => condToExp (env, e1, e2, e3)
                  | AST.AppExp(e1, e2) => let
                      val (fExp, args) = gatherArgs (e1, [e2])
                      (* fall-back function for non-primitive/non-constructor
                       * applications
                       *)
                      fun expRHS () = appToExp (env, fExp, args, fn e => e)
                      fun mkLet rhs = let
                            val x = newTmpVar (cvtTy (typeOfExp exp))
                            in
                              S.mkLET(x, rhs, S.mkRET(S.V_VAR x))
                            end
                      in
                        case repOfExp fExp
                         of AST.VarExp f => (case (B.lookup f, args)
                               of (B.PrimOp p, [arg]) =>
                                    argToValues (env, Prim.arityOf p, arg, fn vs =>
                                      primToRHS (p, vs,
                                       fn S.R_EXP e => e
                                        | rhs => mkLet rhs))
                                | (B.CondOp tst, [arg]) =>
                                    (* application of conditional operator outside
                                     * of a conditional context.
                                     *)
                                    argToValues (env, PrimCond.arityOf tst, arg, fn vs =>
                                      S.mkIF(tst, vs,
                                        S.mkRET(S.V_CON B.conTrue),
                                        S.mkRET(S.V_CON B.conFalse)))
                                | (B.RTFun cf, [arg]) =>
                                    argToValues (env, Runtime.arityOf cf, arg, fn vs =>
                                      mkLet (S.R_CALL(cf, vs)))
                                | (B.UserVar, _) => expRHS ()
                                | _ => raise Fail "bogus application"
                              (* end case *))
                          | AST.ConExp dc => conAppToRHS (env, dc, args, mkLet)
                          | _ => expRHS ()
                        (* end case *)
                      end
                  | AST.CaseExp(e, rules) => expToValue (env, e, fn (v : S.value) => (
                      (* the rule-patterns are either a single tuple pattern or
                       * else a list of data constructor patterns, possibly
                       * ending in a variable pattern.
                       *)
                      case rules
                       of [AST.CaseRule(AST.TuplePat xs, act)] => let
                            val S.V_VAR tpl = v
                            (* convert the tuple deconstruction to a sequence of Selects *)
                            fun cvt (_, [], env) = expToExp (env, act)
                              | cvt (i, p::ps, env) = let
                                  val (x', env') = cvtVarPat (env, p)
                                  in
                                    S.mkLET(x', S.R_SELECT(i, tpl),
                                      cvt (i+1, ps, env'))
                                  end
                            in
                              cvt (0, xs, env)
                            end
                        | [AST.CaseRule(AST.VarPat x, act)] => ( (* trivial case *)
                            case v
                             of S.V_VAR x' => expToExp (bindVar(env, x, x'), act)
                              | _ => let
                                  val (x', env') = cvtVar (env, x)
                                  in
                                    S.mkLET(x', S.R_EXP(S.mkRET(v)),
                                      expToExp(env', act))
                                  end
                            (* end case *))
                        | _ => let
                            (* in this case, the argument must be an instance
                             * of a user-defined type
                             *)
                            val Type.ConTy(tyc, _) = typeOfExp e
                            val exp' = S.mkCASE(v, List.map (ruleToRule env) rules)
                            (* get the case information for `tyc` *)
                            val dcs = SimplifyType.consOf repInfo tyc
                            val (nn, nf) = List.foldl
                                  (fn (dc, (nn, nf)) =>
                                      if SDCon.isNullary dc then (nn+1, nf) else (nn, nf+1))
                                    (0, 0) (SimplifyType.consOf repInfo tyc)
                            val caseInfo = {
                                    ty = cvtTy(typeOfExp e),
                                    argKind = PTy.kindOf(SimplifyType.tycRepOf repInfo tyc),
                                    nConsts = nn,
                                    nFuns = nf
                                  }
                            in
                              (* record the domain of the case for future use *)
                              CaseInfo.set (exp', caseInfo);
                              exp'
                            end
                      (* end case *)))
                  | AST.BindExp(bind, exp) =>
                      bindToExp (env, bind, fn env' => expToExp (env', exp))
                  | _ => expToValue (env, exp, fn v => S.mkRET(v))
                (* end case *))
          (* translate ValBind/FunBind declarations to expressions *)
          and bindToExp (env, dcl, k : env -> S.exp) = (case dcl
                 of AST.FunBind(f, [AST.VarPat x], body) => let
                      val retTy = cvtTy (typeOfExp body)
                      val (f', env') = cvtVar (env, f)
                      (* convert the parameter to SimpleAST and extend the environment *)
                      val (x', env'') = cvtVar (env', x)
                      val body' = expToExp (env'', body)
                      in
                        S.mkFUN(retTy, f', [x'], body', k env')
                      end
                  | AST.FunBind _ => raise Fail "ill-formed function definition"
                  | AST.ValBind(AST.TuplePat[], e) => let
                      val lhs = SVar.new("un", PTy.unitTy)
                      in
                        expToRHS (env, e, fn rhs => S.mkLET(lhs, rhs, k env))
                      end
                  | AST.ValBind(AST.TuplePat ps, e) => let
                      (* convert `let (x1, ..., xn) =  e; ...` to
                       * to `let tpl = e; let x1 = #0(tpl); ...
                       *)
                      val xs = List.map (fn (AST.VarPat x) => x) ps
                      val tpl' = SVar.new("tpl", PTy.ptrTy)
                      fun mkSelect (_, [], env) = k env
                        | mkSelect (i, x::xs, env) = let
                            val (x', env') = cvtVar (env, x)
                            in
                              S.mkLET(x', S.R_SELECT(i, tpl'), mkSelect(i+1, xs, env'))
                            end
                      in
                        (* NOTE: the SimpleAST tuple selection is 0 based *)
                        expToRHS (env, e,
                          fn rhs => S.mkLET(tpl', rhs, mkSelect(0, xs, env)))
                      end
                  | AST.ValBind(AST.VarPat x, e) => let
                      val (x', env') = cvtVar (env, x)
                      in
                        expToRHS (env, e, fn rhs => S.mkLET(x', rhs, k env'))
                      end
                  | AST.ValBind _ => raise Fail "ill-formed value binding"
                (* end case *))
          (* translate AST case rules to SimpleAST *)
          and ruleToRule env (AST.CaseRule(pat, exp)) = (case pat
                 of AST.ConPat(dc, vpats) => let
                      val (xs', env') = List.foldr
                            (fn (p, (xs, env)) => let val (x', env') = cvtVarPat(env, p)
                              in (x'::xs, env') end)
                              ([], env) vpats
                      in
                        (S.P_DCON(cvtDCon dc, xs'), expToExp (env', exp))
                      end
                  | AST.TuplePat _ => raise Fail "impossible: TuplePat"
                  | AST.VarPat x => let
                      val (x', env') = cvtVar (env, x)
                      in
                        (S.P_VAR x', expToExp(env', exp))
                      end
                (* end case *))
          and cvtVarPat (env, AST.VarPat x) = cvtVar (env, x)
            | cvtVarPat _ = raise Fail "expected VarPat"
          (* define the `arguments` parameter *)
          val (arguments', env) = cvtVar (emptyEnv, Basis.varArguments)
          (* the program body has type Unit, but we want to avoid a tail call
           * at the end of the main function, since it is called by C.
           *)
          fun cvtTopDcls (_, []) = S.mkRET(S.V_INT 0)
            | cvtTopDcls (env, AST.TypeDcl _ :: dcls) = cvtTopDcls (env, dcls)
            | cvtTopDcls (env, AST.ValDcl bind :: dcls) =
                bindToExp (env, bind, fn env' => cvtTopDcls (env', dcls))
          val body' = cvtTopDcls (env, dcls)
          in
            S.PROG(arguments', body')
          end (* translate *)

    fun translate prog = let
          val simple = simplify (NormalizeAST.transform prog)
          in
            Census.init simple;
            simple
          end

  end
