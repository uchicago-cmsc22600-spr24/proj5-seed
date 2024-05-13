(* chk-exp.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *)

structure ChkExp : sig

    (* convert a binding-tree expression to an AST expression, while checkinga
     * that it is well typed.s
     *)
    val check : Context.t * BindTree.exp -> AST.exp

    (* type check a value binding while converting it to AST *)
    val chkValBind : Context.t * BindTree.bind -> AST.bind

  end = struct

    structure BT = BindTree
    structure C = Context
    structure Cov = Coverage
    structure Ty = Type
    structure TU = TypeUtil
    structure U = Unify

    (* an expression/type pair for when there is an error *)
    val bogusExp = AST.E(AST.TupleExp[], Ty.ErrorTy)

    fun check (cxt, exp) = (case exp
           of BT.MarkExp m => check (C.withMark (cxt, m))
            | BT.IfExp(e1, e2, e3) => let
		val e1' = check (cxt, e1)
		val e2' = check (cxt, e2)
		val e3' = check (cxt, e3)
		in
		  if not(U.unify(Exp.typeOf e1', Basis.tyBool))
		    then C.error (cxt, ["type of conditional not bool"])
		    else ();
		  if not(U.unify(Exp.typeOf e2', Exp.typeOf e3'))
		    then (
		      C.error (cxt, ["types of then and else clauses must match"]);
		      bogusExp)
		    else Exp.mkIf(e1', e2', e3')
		end
            | BT.OrElseExp(e1, e2) => let
		val e1' = check (cxt, e1)
		val e2' = check (cxt, e2)
		in
		  if not(U.unify(Exp.typeOf e1', Basis.tyBool) andalso
                         U.unify(Exp.typeOf e2', Basis.tyBool))
		    then C.error (cxt, ["arguments of `||` must have type bool"])
		    else ();
		  Exp.mkIf(e1', Exp.mkBoolConst true, e2')
		end
            | BT.AndAlsoExp(e1, e2) => let
		val e1' = check (cxt, e1)
		val e2' = check (cxt, e2)
		in
		  if not(U.unify(Exp.typeOf e1', Basis.tyBool) andalso
                         U.unify(Exp.typeOf e2', Basis.tyBool))
		    then C.error (cxt, ["arguments of `&&` must have type bool"])
		    else ();
		  Exp.mkIf(e1', e2', Exp.mkBoolConst false)
		end
            | BT.BinExp(e1, oper, e2) => let
		val e1' = check (cxt, e1)
		val e2' = check (cxt, e2)
                val oper' = IdProps.var oper
                (* the operator's type *)
                val ty = TU.instantiate (Var.typeOf oper', C.depthOf cxt)
                val SOME(argTy, resTy) = Ty.asFunTy ty
                in
                  if not(U.unify(argTy, Ty.tupleTy[Exp.typeOf e1', Exp.typeOf e2']))
                    then C.error (cxt, [
                        "type mismatch for operator '",
                        BindTree.ValId.nameOf oper, "'"
                      ])
                    else ();
                  AST.E(AST.AppExp(
                    AST.E(AST.VarExp oper', ty), Exp.mkTuple[e1', e2']),
                    resTy)
                end
            | BT.UnExp(oper, e) => let
		val e' = check (cxt, e)
                val oper' = IdProps.var oper
                (* the operator's type *)
                val ty = TU.instantiate (Var.typeOf oper', C.depthOf cxt)
                val SOME(argTy, resTy) = Ty.asFunTy ty
                in
                  if not(U.unify(argTy, Exp.typeOf e'))
                    then C.error (cxt, [
                        "type mismatch for operator '",
                        BindTree.ValId.nameOf oper, "'"
                      ])
                    else ();
                  AST.E(AST.AppExp(AST.E(AST.VarExp oper', ty), e'), resTy)
                end
            | BT.AppExp(e1, e2) => let
		val e1' = check (cxt, e1)
		val e2' = check (cxt, e2)
		val resTy = Ty.metaVarTy (C.depthOf cxt)
		in
		  if not(U.unify(Exp.typeOf e1', Ty.funTy(Exp.typeOf e2', resTy)))
		    then C.error (cxt, ["type mismatch in application"])
		    else ();
		  AST.E(AST.AppExp(e1', e2'), resTy)
		end
            | BT.VarExp x => let
                val x' = IdProps.var x
                val ty = TU.instantiate (Var.typeOf x', C.depthOf cxt)
                in
                  AST.E(AST.VarExp x', ty)
                end
            | BT.ConExp dc => let
                val dc' = IdProps.dcon dc
                val ty = TU.instantiate (DataCon.typeOf dc', C.depthOf cxt)
                in
                  AST.E(AST.ConExp dc', ty)
                end
            | BT.IntExp n =>
              (* integer literals are always positive, but they have to be less
               * than 2^62.
               *)
                if (n < 0x4000000000000000)
                  then Exp.mkInt n
                  else (
                    C.error (cxt, [
                        "integer literal ", IntInf.toString n, " too large"
                      ]);
                    bogusExp)
            | BT.StrExp s => Exp.mkStr s
            | BT.TupleExp es =>	Exp.mkTuple (List.map (fn e => check(cxt, e)) es)
            | BT.CaseExp(e, rules) => let
                val e' = check (cxt, e)
                val rules' = chkRules (cxt, Exp.typeOf e', rules)
                in
                  Exp.mkCase(e', rules')
                end
            | BT.BindExp(bind, e) => let
                val bind' = chkValBind (cxt, bind)
                in
                  Exp.mkBind(bind', check(cxt, e))
                end
          (* end case *))

    (* typecheck a list of case-expression rules (including coverage checking) *)
    and chkRules (cxt, argTy, rules) = let
          val resTy = Ty.metaVarTy (C.depthOf cxt)
          fun chkRule (cxt, BT.MarkRule m) = chkRule (C.withMark (cxt, m))
            | chkRule (cxt, BT.CaseRule(p, e)) = let
                val p' = ChkPat.check (cxt, argTy, p)
                val e' = check (cxt, e)
                in
                  (* check that the result type of the rule is compatible with
                   * the previous rules.
                   *)
                  if not (U.unify (Exp.typeOf e', resTy))
                    then C.error (cxt, ["inconsistent result type for case rule"])
                    else ();
                  (C.spanOf cxt, (p', e'))
                end
          val rules' = List.map (fn r => chkRule (cxt, r)) rules
          (* once we have done the type checking, we can check for coverage *)
          fun chkCover ([], rules', cover) = (
                if not(Cov.exhaustive cover)
                  then C.error(cxt, ["non-exhaustive case expression"])
                  else ();
                List.rev rules')
            | chkCover ((span, (p, e)) :: rules, rules', cover) = let
                val (cover, isRedundant) = Cov.update(cover, p)
                in
                  if isRedundant
                    then C.error(C.setSpan(cxt, span), ["redundant pattern in case"])
                    else ();
                  chkCover (rules, AST.CaseRule(p, e)::rules', cover)
                end
          in
            chkCover (rules', [], Cov.init (Ty.prune argTy))
          end
(*DEBUG*)handle ex => raise ex

    and chkValBind (cxt, BT.MarkBind m) = chkValBind (C.withMark (cxt, m))
      | chkValBind (cxt, BT.FunBind(f, params, body)) = let
          val cxt' = C.incDepth cxt
          val resTy = Ty.metaVarTy (C.depthOf cxt')
          (* check a parameter pattern; this function is designed to be applied
           * the the function parameters in right-to-left order.
           *)
          fun chkParam (p, (ps', resTy)) = let
                val paramTy = Ty.metaVarTy (C.depthOf cxt')
                val p' = ChkPat.check (cxt', paramTy, p)
                in
                  (p'::ps', Ty.funTy(Ty.prune paramTy, resTy))
                end
          val (params', fnTy) = List.foldr chkParam ([], resTy) params
          val f' = Var.new (f, fnTy)
          val _ = IdProps.varSet (f, f')
          val body' = check (cxt', body)
          in
            if not (U.unify(resTy, Exp.typeOf body'))
              then C.error (cxt, ["type mismatch in function body"])
              else Var.updateTy (f', TU.closeTy (fnTy, C.depthOf cxt));
            AST.FunBind(f', params', body')
          end
      | chkValBind (cxt, BT.ValBind(lhs, rhs)) = let
          val lhsTy = Ty.metaVarTy (C.depthOf cxt)
          val lhs' = ChkPat.check (cxt, lhsTy, lhs)
          val rhs' = check (cxt, rhs)
          in
            AST.ValBind(lhs', rhs')
          end
      | chkValBind (cxt, BT.DoExpBind e) = let
          val e' = check (cxt, e)
          in
            if not (U.unify (Exp.typeOf e', Basis.tyUnit))
              then C.error (cxt, ["expected 'Unit' type for do-expression"])
              else ();
            AST.ValBind(AST.TuplePat[], e')
          end

  end
