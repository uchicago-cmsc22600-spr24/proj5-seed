(* normalize-ast.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * This structure implements a pass that cleans up the AST IR prior to simplification.
 * The resulting AST will have the following invariants:
 *      - data-constructor functions will always be applied to all of their arguments
 *      - built-in functions will always be applied to arguments
 *      - function bindings will involve only one level of parameters (i.e., curried
 *        bindings will have been converted to nested functions) and the function
 *        parameter will be a single variable
 *      - ConPat and TuplePat will have variables (VarPat) as arguments
 *)

structure NormalizeAST : sig

    val transform : AST.program -> AST.program

  end = struct

    structure Ty = Type
    structure DC = DataCon

    (* given a data constructor that appears in an argument position, convert it to
     * a function abstraction (if necessary).
     *)
    fun cvtCon (dc, ty) = (case Ty.asFunTy ty
           of SOME(argTy, resTy) => let
(* need to define a curried function *)
                val f = Var.newTmp(DC.nameOf dc, ty)
                val x = Var.newTmp("x", argTy)
                in
                  Exp.mkBind(
                    AST.FunBind(f, [AST.VarPat x],
                      AST.E(AST.AppExp(
                        AST.E(AST.ConExp dc, ty),
                        AST.E(AST.VarExp x, argTy)), resTy)),
                    AST.E(AST.VarExp f, Ty.funTy(argTy, resTy)))
                end
            | NONE => AST.E(AST.ConExp dc, ty)
          (* end case *))

    (* given a built-in function that is being used in an argument position,
     * convert it to a function abstraction.
     *)
    fun cvtPrim (prim, ty) = (case Ty.asFunTy ty
           of SOME(argTy, resTy) => let
                val f = Var.newTmp(Var.nameOf prim, ty)
                val x = Var.newTmp("x", argTy)
                in
                  Exp.mkBind(
                    AST.FunBind(f, [AST.VarPat x],
                      AST.E(AST.AppExp(
                        AST.E(AST.VarExp prim, ty),
                        AST.E(AST.VarExp x, argTy)), resTy)),
                    AST.E(AST.VarExp f, Ty.funTy(argTy, resTy)))
                end
            | _ => AST.E(AST.VarExp prim, ty)
          (* end case *))

    (* transform the program *)
    fun transform (AST.Prog dcls) = let
          fun xformExp (exp as AST.E(rep, ty)) = let
                fun mk rep' = AST.E(rep', ty)
                in
                  case rep
                   of AST.BindExp(bnd, e) =>
                        mk (AST.BindExp(xformBind bnd, xformExp e))
                    | AST.IfExp(e1, e2, e3) =>
                        mk (AST.IfExp(xformExp e1, xformExp e2, xformExp e3))
                    | AST.CaseExp(e, rules) =>
                        mk (AST.CaseExp(xformExp e, List.map xformRule rules))
                    | AST.AppExp(e1, e2) => xformApp (e1, [e2])
                    | AST.TupleExp es => mk (AST.TupleExp(List.map xformExp es))
                    | AST.ConExp dc => cvtCon(dc, ty)
                    | AST.VarExp x => if Var.isPrim x
                        then cvtPrim (x, ty)
                        else exp
                    | _ => exp
                  (* end case *)
                end
          and xformApp (AST.E(AST.AppExp(e1, e2), _), args) = xformApp (e1, e2::args)
            | xformApp (e1 as AST.E(AST.ConExp dc, ty), args) = let
                (* build up an application expression *)
                val appExp = List.foldl
                      (fn (arg, e) => Exp.mkApp(e, xformExp arg))
                        e1 args
                val missingArgs = DC.arityOf dc - List.length args
                in
                  if (missingArgs = 0)
                    (* `appExp` is a complete application of `dc` to arguments *)
                    then appExp
                  else if (missingArgs < 0)
                    then raise Fail "invalid constructor application"
                    (* `appExp` is a partial application of `dc` to arguments *)
                    else let
                      fun mkAbs (0, e) = e
                        | mkAbs (n, e) = (case Ty.asFunTy(Exp.typeOf e)
                             of SOME(argTy, resTy) => let
                                  fun mkBody arg =
                                        mkAbs(n-1, Exp.mkApp(e, Exp.mkVar(arg, argTy)))
                                  in
                                    Exp.lambda(argTy, resTy, mkBody)
                                  end
                              | NONE => raise Fail "expected function type"
                            (* end case *))
                      in
                        mkAbs (missingArgs, appExp)
                      end
                end
            | xformApp (f, args) =
                List.foldl (fn (arg, e) => Exp.mkApp(e, xformExp arg)) f args
          and xformBind (AST.FunBind(f, p1::ps, body)) = let
                val name = Var.nameOf f
                val Ty.TyScm(_, fnTy) = Var.typeOf f
                val SOME(_, resTy) = Ty.asFunTy fnTy
                (* make a function binding of a single variable pattern
                 * for the parameters
                 *)
                fun mkFunBind (f, p, e) = let
                      val (p', e') = (case p
                             of AST.TuplePat ps => let
                                  val (x, e') = xformTuplePat (ps, e)
                                  in
                                    (AST.VarPat x, e')
                                  end
                              | AST.VarPat _ => (p, e)
                              | _ => raise Fail "ill formed parameter pattern"
                            (* end case *))
                      in
                        AST.FunBind(f, [p'], e')
                      end
                (* convert curried bindings to nested function definitions *)
                fun nestCurriedFn (_, [], _) = xformExp body
                  | nestCurriedFn (i, p::ps, ty) = (case Ty.asFunTy ty
                       of SOME(_, resTy) => let
                            val f' = Var.newTmp(name ^ Int.toString i, ty)
                            in
                              Exp.mkBind(
                                mkFunBind(f', p, nestCurriedFn (i+1, ps, resTy)),
                                AST.E(AST.VarExp f', ty))
                            end
                        | NONE => raise Fail "badly formed function binding"
                      (* end case *))
                in
                  mkFunBind(f, p1, nestCurriedFn (1, ps, resTy))
                end
            | xformBind (AST.ValBind(p, e)) = AST.ValBind(p, xformExp e)
            | xformBind _ = raise Fail "ill-formed binding"
          and xformRule (AST.CaseRule(p, e)) = AST.CaseRule(p, xformExp e)
          and xformTuplePat (ps, e) = let
                (* arguments to tuple patterns are always variables *)
                fun tyOfPat (AST.VarPat x) = let
                      val Ty.TyScm(_, ty) = Var.typeOf x
                      in
                        ty
                      end
                  | tyOfPat _ = raise Fail "expected VarPat"
                val ty = Ty.tupleTy (List.map tyOfPat ps)
                val tpl = Var.newTmp("tpl", ty)
                val e' = Exp.mkCase(AST.E(AST.VarExp tpl, ty), [
                        AST.CaseRule(AST.TuplePat ps, e)
                      ])
                in
                  (tpl, e')
                end
          (* transform top-level declarations *)
          fun xformTopDcl (AST.ValDcl vbind) = AST.ValDcl(xformBind vbind)
            | xformTopDcl dcl = dcl
          in
            AST.Prog(List.map xformTopDcl dcls)
          end

  end
