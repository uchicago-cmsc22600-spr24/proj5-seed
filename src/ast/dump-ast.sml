(* dump-ast.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Dump the AST data structure in S-Expression syntax.
 *)

(* we map type constructors, data constructors, and variables to unique IDs
 * to check the correctness of bindings.
 *)
functor IdGen (Tbl : MONO_HASH_TABLE) : sig
    type item = Tbl.Key.hash_key
    type id = SExp.value
  (* create a new item to ID mapping *)
    val new : unit -> item -> id
  end = struct

    type item = Tbl.Key.hash_key
    type id = SExp.value

    fun new () = let
          val nextId = ref 0
          val idTbl = Tbl.mkTable (32, Fail "IdGen")
          val find = Tbl.find idTbl
          val ins = Tbl.insert idTbl
          in
            fn item => (case find item
                 of NONE => let
                      val id = !nextId
                      val sexp = SExp.INT(IntInf.fromInt id)
                      in
                        nextId := id + 1;
                        ins (item, sexp);
                        sexp
                      end
                  | SOME id => id
                (* end case *))
          end

  end (* functor IdGen *)

structure DumpAST : sig

  (* `dumpToFile (stem, ast)` dumps the abstract syntax tree `ast` to `stem.ast`. *)
    val dumpToFile : string * AST.program -> unit

  end = struct

    structure T = AST
    structure Ty = Type
    structure Tyc = TyCon

    datatype sexp = datatype SExp.value

  (* Generators for mapping various kinds of identifiers to unique IDs.  Note
   * that we include meta variables here.  Unique IDs are assigned in a
   * left-to-right pre-order traversal of binding sites to ensure a canonical
   * representation.
   *)
    structure TyVarIdGen = IdGen (TyVar.Tbl)
    structure MetaVarIdGen = IdGen (MetaVar.Tbl)
    structure TycIdGen = IdGen (Tyc.Tbl)
    structure ConIdGen = IdGen (DataCon.Tbl)
    structure VarIdGen = IdGen (Var.Tbl)

    type env = {
        idOfTV : TyVar.t -> sexp,
        idOfMV : MetaVar.t -> sexp,
        idOfTyc : Tyc.t -> sexp,
        idOfCon : DataCon.t -> sexp,
        idOfVar : Var.t -> sexp
      }

    fun newEnv () : env = {
            idOfTV = TyVarIdGen.new(),
            idOfMV = MetaVarIdGen.new(),
            idOfTyc = TycIdGen.new(),
            idOfCon = ConIdGen.new(),
            idOfVar = VarIdGen.new()
          }

  (* initialize the ID map for the initial basis *)
    fun initEnv () = let
          val env as {idOfTyc, idOfTV, idOfCon, idOfVar, ...} = newEnv ()
          fun registerTyc tyc = ignore(idOfTyc tyc)
          fun registerCon dc = ignore(idOfCon dc)
          fun registerVar x = ignore(idOfVar x)
          in
          (* Basis type constructors *)
            registerTyc Basis.tycBool;
            registerTyc Basis.tycInt;
            registerTyc Basis.tycList;
            registerTyc Basis.tycString;
            registerTyc Basis.tycUnit;
          (* Basis data constructors *)
            registerCon Basis.conTrue;
            registerCon Basis.conFalse;
            registerCon Basis.conCons;
            registerCon Basis.conNil;
          (* operators *)
            registerVar Basis.opEQL;
            registerVar Basis.opNEQ;
            registerVar Basis.opLTE;
            registerVar Basis.opLT;
            registerVar Basis.opADD;
            registerVar Basis.opSUB;
            registerVar Basis.opMUL;
            registerVar Basis.opDIV;
            registerVar Basis.opMOD;
            registerVar Basis.opNEG;
          (* other variables *)
            registerVar Basis.varArguments;
            registerVar Basis.varChr;
            registerVar Basis.varExit;
            registerVar Basis.varFail;
            registerVar Basis.varNewRef;
            registerVar Basis.varPrint;
            registerVar Basis.varSize;
            registerVar Basis.varSub;
            env
          end

  (* walk the program and assign unique IDs for bound identifiers *)
    fun assignIds (T.Prog dcls) = let
          val env = initEnv ()
          fun assignTVId tv = ignore (#idOfTV env tv)
          fun assignMVId mv = ignore (#idOfMV env mv)
          fun assignTycId tyc = ignore (#idOfTyc env tyc)
          fun assignConId dc = ignore (#idOfCon env dc)
          fun assignVarId x = ignore (#idOfVar env x)
          fun doDcl (T.TypeDcl usrTyc) = doUserTyc usrTyc
            | doDcl (T.ValDcl bind) = doBind bind
          and doUserTyc (usrTyc : Ty.usr_tyc) = (
                assignTycId (Ty.UserTyc usrTyc);
                List.app assignTVId (#params usrTyc);
                List.app doDCon (! (#cons usrTyc)))
          and doDCon (dc as Ty.DCon{argTys, ...}) = (
                assignConId dc;
                List.app doTy argTys)
          and doTy ty = (case Ty.prune ty
                 of Ty.VarTy tv => ()
                  | Ty.MetaTy mv => assignMVId mv
                  | Ty.ConTy(_, tys) => List.app doTy tys
                  | Ty.ErrorTy => ()
                (* end case *))
          and doExp (T.E(e, ty)) = (
                doTy ty;
                case e
                 of T.IfExp(e1, e2, e3) => (doExp e1; doExp e2; doExp e3)
                  | T.AppExp(e1, e2) => (doExp e1; doExp e2)
                  | T.TupleExp es => List.app doExp es
                  | T.CaseExp(e, rules) => (doExp e; List.app doRule rules)
                  | T.BindExp(bind, e) => (doBind bind; doExp e)
                  | _ => ()
                (* end case *))
          and doBind (T.FunBind(f, ps, e)) = (
                assignVarId f;
                List.app doPat ps;
                doExp e)
            | doBind (T.ValBind(p, e)) = (doPat p; doExp e)
          and doRule (T.CaseRule(p, e)) = (doPat p; doExp e)
          and doPat (T.ConPat(_, ps)) = List.app doPat ps
            | doPat (T.TuplePat ps) = List.app doPat ps
            | doPat (T.VarPat x) = assignVarId x
          in
            List.app doDcl dcls;
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
    val mkProg = mkNode "Prog"
    val mkTypeDcl = mkNode "TypeDcl"
    val mkDCon = mkNode "DCon"
    val mkValDcl = mkNode "ValDcl"
    val mkFunBind = mkNode "FunBind"
    val mkValBind = mkNode "ValBind"
    val mkVarTy = mkNode "VarTy"
    val mkMetaTy = mkNode "MetaTy"
    val mkConTy = mkNode "ConTy"
    val mkFnTy = mkNode "FnTy"
    val errorTy = SYMBOL(Atom.atom "ErrorTy")
    val mkE = mkNode "E"
    val mkIfExp = mkNode "IfExp"
    val mkAppExp = mkNode "AppExp"
    val mkOvldExp = mkNode "OvldExp"
    val mkVarExp = mkNode "VarExp"
    val mkConExp = mkNode "ConExp"
    val mkIntExp = mkNode "IntExp"
    val mkStrExp = mkNode "StrExp"
    val mkTupleExp = mkNode "TupleExp"
    val mkCaseExp = mkNode "CaseExp"
    val mkBindExp = mkNode "BindExp"
    val mkCaseRule = mkNode "CaseRule"
    val mkConPat = mkNode "ConPat"
    val mkTuplePat = mkNode "TuplePat"
    val mkVarPat = mkNode "VarPat"
    end (* local *)

    fun list f xs = LIST(List.map f xs)

  (* convert basic values to an S-expression *)
    fun tv2sexp (env : env) tv = LIST[STRING(TyVar.nameOf tv), #idOfTV env tv]
    fun tyc2sexp (env : env) tyc = LIST[STRING(Tyc.nameOf tyc), #idOfTyc env tyc]
    fun dc2sexp (env : env) dc = LIST[STRING(DataCon.nameOf dc), #idOfCon env dc]
    fun var2sexp (env : env) x = LIST[STRING(Var.nameOf x), #idOfVar env x]

    fun prog2sexp (env : env) (T.Prog dcls) = mkProg (List.map (dcl2sexp env) dcls)

    and dcl2sexp (env : env) (T.TypeDcl usrTyc) = mkTypeDcl [
            SYMBOL(Atom.atom(#name usrTyc)),
            list (tv2sexp env) (#params usrTyc),
            list (con2sexp env) (! (#cons usrTyc))
          ]
      | dcl2sexp (env : env) (T.ValDcl bind) = mkValDcl [bind2sexp env bind]

    and con2sexp (env : env) dc = (case DataCon.tySigOf dc
           of (_, [], _) => mkDCon [dc2sexp env dc]
            | (_, tys, _) => mkDCon (dc2sexp env dc :: List.map (ty2sexp env) tys)
          (* end case *))

    and tyscm2sexp (env : env) (Ty.TyScm([], ty)) = ty2sexp env ty
      | tyscm2sexp env (Ty.TyScm(tvs, ty)) = LIST [
            STRING "FORALL", list (tv2sexp env) tvs, ty2sexp env ty
          ]

    and ty2sexp (env : env) ty = (case Ty.prune ty
           of Ty.VarTy tv => mkVarTy [tv2sexp env tv]
            | Ty.MetaTy mv => mkMetaTy [#idOfMV env mv]
            | Ty.ConTy(tyc, tys) => mkConTy [tyc2sexp env tyc, list (ty2sexp env) tys]
            | Ty.ErrorTy => errorTy
          (* end case *))

    and exp2sexp (env : env) (T.E(e, ty)) : SExp.value = (case e
           of T.IfExp(e1, e2, e3) => mkIfExp [
                  exp2sexp env e1, exp2sexp env e2, exp2sexp env e3
                ]
            | T.AppExp(e1, e2) => mkAppExp [exp2sexp env e1, exp2sexp env e2]
            | T.VarExp x => mkVarExp [var2sexp env x]
            | T.ConExp dc => mkConExp [dc2sexp env dc]
            | T.IntExp n => mkIntExp [INT n]
            | T.StrExp s => mkStrExp [STRING s]
            | T.TupleExp es => mkTupleExp (List.map (exp2sexp env) es)
            | T.CaseExp(e, rules) =>
                mkCaseExp [exp2sexp env e, list (rule2sexp env) rules]
            | T.BindExp(bind, e) => mkBindExp[bind2sexp env bind, exp2sexp env e]
          (* end case *))

    and bind2sexp (env : env) (T.FunBind(f, ps, e)) = mkFunBind (
          var2sexp env f :: STRING ":" :: tyscm2sexp env (Var.typeOf f)
            :: List.map (pat2sexp env) ps
            @ [exp2sexp env e])
      | bind2sexp (env : env) (T.ValBind(p, e)) =
          mkValBind [pat2sexp env p, exp2sexp env e]

    and rule2sexp (env : env) (T.CaseRule(p, e)) =
          mkCaseRule [pat2sexp env p, exp2sexp env e]

    and pat2sexp (env : env) p = (case p
           of T.ConPat(dc, ps) => mkConPat (dc2sexp env dc :: List.map (pat2sexp env) ps)
            | T.TuplePat xs => mkTuplePat (List.map (pat2sexp env) xs)
            | T.VarPat x => mkVarPat [var2sexp env x]
          (* end case *))

    fun dumpToFile (stem, prog) =
          DumpUtil.dump "ast" (stem, prog2sexp (assignIds prog) prog)

  end
