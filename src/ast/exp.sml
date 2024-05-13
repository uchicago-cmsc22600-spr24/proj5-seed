(* exp.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Utility code for dealing with expressions in the AST IR.
 *)

structure Exp : sig

    val typeOf : AST.exp -> Type.ty

    val repOf : AST.exp -> AST.exp_rep

    (* "smart" constructors that figure out the type of the expression *)
    val mkBind : AST.bind * AST.exp -> AST.exp
    val mkFun : Var.t * AST.pat list * AST.exp * AST.exp -> AST.exp
    val mkVal : AST.pat * AST.exp * AST.exp -> AST.exp
    val mkIf : AST.exp * AST.exp * AST.exp -> AST.exp
    val mkApp : AST.exp * AST.exp -> AST.exp
    val mkCase : AST.exp * AST.rule list -> AST.exp
    val mkTuple : AST.exp list -> AST.exp
    val mkVar : Var.t * Type.ty -> AST.exp
    val mkInt : IntInf.int -> AST.exp
    val mkStr : string -> AST.exp

    val mkBoolConst : bool -> AST.exp

    (* create a function valued expression *)
    val lambda : Type.ty * Type.ty * (Var.t -> AST.exp) -> AST.exp

  end = struct

    datatype exp = datatype AST.exp
    datatype exp_rep = datatype AST.exp_rep

    fun typeOf (E(_, ty)) = ty
    fun repOf (E(rep, _)) = rep

    fun mkBind (bind, e) = E(BindExp(bind, e), typeOf e)
    fun mkFun (f, params, body, e) = mkBind(AST.FunBind(f, params, body), e)
    fun mkVal (x, e1, e2) = mkBind(AST.ValBind(x, e1), e2)

    fun mkIf (e1, e2, e3) = E(IfExp(e1, e2, e3), typeOf e2)

    fun mkCase (_, []) = raise Fail "empty case"
      | mkCase (arg, rules as AST.CaseRule(_, e)::_) = E(CaseExp(arg, rules), typeOf e)

    fun mkApp (e1, e2) = (case Type.asFunTy(typeOf e1)
           of SOME(_, resTy) => E(AppExp(e1, e2), resTy)
            | NONE => raise Fail "expected function type"
          (* end case *))

    fun mkTuple [] = E(TupleExp[], Basis.tyUnit)
      | mkTuple [e] = e
      | mkTuple es = E(TupleExp es, Type.tupleTy(List.map typeOf es))

    fun mkVar (x, ty) = E(AST.VarExp x, ty)
    fun mkInt n = E(AST.IntExp n, Basis.tyInt)
    fun mkStr s = E(AST.StrExp s, Basis.tyString)

    fun mkBoolConst b = E(
          ConExp(if b then Basis.conTrue else Basis.conFalse),
          Basis.tyBool)

    fun lambda (argTy, resTy, mkBody) = let
          val arg = Var.newTmp("_arg", argTy)
          val fnTy = Type.funTy(argTy, resTy)
          val f = Var.newTmp("_fn", fnTy)
          in
            mkFun(f, [AST.VarPat arg], mkBody arg, mkVar(f, fnTy))
          end

  end
