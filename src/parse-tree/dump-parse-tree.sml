(* dump-parse-tree.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Dump the ParseTree data structure in S-Expression syntax.
 *)

structure DumpParseTree : sig

    val dumpToFile : string * bool * ParseTree.program -> unit

  end = struct

    structure PT = ParseTree

    datatype sexp = datatype SExp.value

  (* helper functions to construct the S-Expression `(sym ...)` *)
    local
      fun mkNode name = let
	    val sym = SYMBOL(Atom.atom name)
	    in
	      fn args => LIST(sym :: args)
	    end
    in
    val mkSPAN = mkNode "SPAN"
    val mkProg = mkNode "Prog"
    val mkTypeDcl = mkNode "TypeDcl"
    val mkValDcl = mkNode "ValDcl"
    val mkFunTy = mkNode "FunTy"
    val mkTupleTy = mkNode "TupleTy"
    val mkConTy = mkNode "ConTy"
    val mkVarTy = mkNode "VarTy"
    val mkCon = mkNode "Con"
    val mkFunBind = mkNode "FunBind"
    val mkValBind = mkNode "ValBind"
    val mkDoExpBind = mkNode "DoExpBind"
    val mkIfExp = mkNode "IfExp"
    val mkOrElseExp = mkNode "OrElseExp"
    val mkAndAlsoExp = mkNode "AndAlsoExp"
    val mkBinExp = mkNode "BinExp"
    val mkUnExp = mkNode "UnExp"
    val mkAppExp = mkNode "AppExp"
    val mkTupleExp = mkNode "TupleExp"
    val mkVarExp = mkNode "VarExp"
    val mkConExp = mkNode "ConExp"
    val mkIntExp = mkNode "IntExp"
    val mkStrExp = mkNode "StrExp"
    val mkCaseExp = mkNode "CaseExp"
    val mkBindExp = mkNode "BindExp"
    val mkCaseRule = mkNode "CaseRule"
    val mkTuplePat = mkNode "TuplePat"
    val mkConPat = mkNode "ConPat"
    val mkVarPat = mkNode "VarPat"
    val wildPat = SYMBOL(Atom.atom "WildPat")
    end (* local *)

  (* convert file position to S-Expression *)
    fun pos2sexp pos = INT(AntlrStreamPos.FilePos.toLarge pos)

  (* conditionally wrap a SPAN around a subtree *)
    fun mark2sexp tree2sexp (false, {span, tree}) = tree2sexp (false, tree)
      | mark2sexp tree2sexp (true, {span=(l, r), tree}) =
	  mkSPAN [pos2sexp l, pos2sexp r, tree2sexp(true, tree)]

    fun prog2sexp (marks, PT.MarkProg m) = mark2sexp prog2sexp (marks, m)
      | prog2sexp (marks, PT.Prog dcls) =
	  mkProg (List.map (fn d => dcl2sexp(marks, d)) dcls)

    and dcl2sexp (marks, dcl) = (case dcl
	   of PT.MarkDcl m => mark2sexp dcl2sexp (marks, m)
	    | PT.TypeDcl(id, tyParams, cons) => mkTypeDcl (
		SYMBOL id ::
		LIST(List.map SYMBOL tyParams) ::
		List.map (fn cd => con2sexp (marks, cd)) cons)
	    | PT.ValDcl bind => bind2sexp (marks, bind)
	  (* end case *))

    and ty2sexp (marks, ty) = let
	  fun toSExp ty = (case ty
		 of PT.MarkTy m => mark2sexp ty2sexp (marks, m)
		  | PT.FunTy(ty1, ty2) => mkFunTy [toSExp ty1, toSExp ty2]
		  | PT.TupleTy tys => mkTupleTy (List.map toSExp tys)
		  | PT.ConTy(tyc, tys) => mkConTy (SYMBOL tyc :: List.map toSExp tys)
		  | PT.VarTy tv => mkVarTy [SYMBOL tv]
		(* end case *))
	  in
	    toSExp ty
	  end

    and con2sexp (marks, con) = (case con
	   of PT.MarkCon m => mark2sexp con2sexp (marks, m)
	    | PT.Con(conid, tys) =>
		mkCon (SYMBOL conid :: List.map (fn ty => ty2sexp (marks, ty)) tys)
	  (* end case *))

    and bind2sexp (marks, bind) = (case bind
	   of PT.MarkBind m => mark2sexp bind2sexp (marks, m)
	    | PT.FunBind(id, pats, exp) => mkFunBind [
		  SYMBOL id,
		  LIST(List.map (fn p => pat2sexp (marks, p)) pats),
		  exp2sexp (marks, exp)
		]
	    | PT.ValBind(pat, exp) => mkValBind [
		  pat2sexp (marks, pat), exp2sexp (marks, exp)
		]
	    | PT.DoExpBind exp => mkDoExpBind [exp2sexp (marks, exp)]
	  (* end case *))

    and exp2sexp (marks, exp) = let
	  fun toSExp exp = (case exp
		 of PT.MarkExp m => mark2sexp exp2sexp (marks, m)
		  | PT.IfExp(e1, e2, e3) => mkIfExp [toSExp e1, toSExp e2, toSExp e3]
		  | PT.OrElseExp(e1, e2) => mkOrElseExp [toSExp e1, toSExp e2]
		  | PT.AndAlsoExp(e1, e2) => mkAndAlsoExp [toSExp e1, toSExp e2]
		  | PT.BinExp(e1, id, e2) => mkBinExp [toSExp e1, SYMBOL id, toSExp e2]
		  | PT.UnExp(id, e) => mkUnExp [SYMBOL id, toSExp e]
		  | PT.AppExp(e1, e2) => mkAppExp [toSExp e1, toSExp e2]
                  | PT.TupleExp es => mkTupleExp (List.map toSExp es)
		  | PT.VarExp id => mkVarExp [SYMBOL id]
		  | PT.ConExp id => mkConExp [SYMBOL id]
		  | PT.IntExp n => mkIntExp [INT n]
		  | PT.StrExp s => mkStrExp [STRING s]
		  | PT.CaseExp(exp, rules) => mkCaseExp [
			toSExp exp, LIST(List.map (fn r => rule2sexp(marks, r)) rules)
		      ]
                  | PT.BindExp(vb, exp) => mkBindExp [bind2sexp(marks, vb), toSExp exp]
		(* end case *))
	  in
	    toSExp exp
	  end

    and rule2sexp (marks, rule) = (case rule
	   of PT.MarkRule m => mark2sexp rule2sexp (marks, m)
	    | PT.CaseRule(pat, exp) => mkCaseRule [
		  pat2sexp (marks, pat), exp2sexp (marks, exp)
		]
	  (* end case *))

    and pat2sexp (marks, pat) = let
	  fun toSExp pat = (case pat
		 of PT.MarkPat m => mark2sexp pat2sexp (marks, m)
                  | PT.TuplePat pats => mkTuplePat (List.map toSExp pats)
		  | PT.ConPat(id, pats) => mkConPat (SYMBOL id :: List.map toSExp pats)
		  | PT.VarPat id => mkVarPat [SYMBOL id]
		  | PT.WildPat => wildPat
		(* end case *))
	  in
	    toSExp pat
	  end

    fun dumpToFile (stem, marks, prog) =
          DumpUtil.dump "pt" (stem, prog2sexp (marks, prog))

  end
