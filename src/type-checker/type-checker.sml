(* type-checker.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * The root of the MiniML type checker.
 *)

structure TypeChecker : sig

    (* Type check a program and convert it to the typed AST representation.
     * Errors are reported on the given error stream.
     *)
    val check : Error.err_stream * BindTree.program -> AST.program

  end = struct

    structure BT = BindTree
    structure C = Context

    (* typecheck a top-level declaration *)
    fun chkDcl (cxt, BT.MarkDcl m) = chkDcl (C.withMark (cxt, m))
      | chkDcl (cxt, BT.TypeDcl(tyc, tvs, cons)) = let
          val tvs' = List.map IdProps.tyvar tvs
          val tyc' = TyCon.new(tyc, tvs')
          (* check a data-constuctor definition; since we add it to the data
           * type when we create the AST version, this function is executed for
           * effect only.
           *)
          fun chkCon (cxt, BT.MarkCon m) = chkCon (C.withMark (cxt, m))
            | chkCon (cxt, BT.Con(dc, tys)) = let
                val tys' = List.map (fn ty => ChkTy.check (cxt, ty)) tys
                val dc' = DataCon.new(tyc', dc, tys')
                in
                  IdProps.dconSet(dc, dc')
                end
          val () = IdProps.tyconSet (tyc, tyc')
          val () = List.app (fn dc => chkCon(cxt, dc)) cons
          val usrTyc = TyCon.finish tyc'
          in
            AST.TypeDcl usrTyc
          end
      | chkDcl (cxt, BT.ValDcl bnd) = AST.ValDcl(ChkExp.chkValBind(cxt, bnd))

    (* typecheck a program *)
    fun chkProg (cxt, BT.MarkProg m) = chkProg (C.withMark (cxt, m))
      | chkProg (cxt, BT.Prog dcls) = let
          val dcls' = List.map (fn dcl => chkDcl (cxt, dcl)) dcls
          in
            AST.Prog dcls'
          end

    (* create the initial mapping from BindTree basis identifiers to AST identifiers *)
    fun initBasis () = (
          List.app IdProps.tyconSet [
	      (BindBasis.tycBool, Basis.tycBool),
	      (BindBasis.tycInt, Basis.tycInt),
	      (BindBasis.tycList, Basis.tycList),
	      (BindBasis.tycRef, Basis.tycRef),
	      (BindBasis.tycString, Basis.tycString),
	      (BindBasis.tycUnit, Basis.tycUnit)
            ];
          List.app IdProps.dconSet [
	      (BindBasis.conTrue, Basis.conTrue),
	      (BindBasis.conFalse, Basis.conFalse),
	      (BindBasis.conCons, Basis.conCons),
	      (BindBasis.conNil, Basis.conNil)
            ];
          List.app IdProps.varSet [
	      (BindBasis.opASSIGN, Basis.opASSIGN),
	      (BindBasis.opEQL, Basis.opEQL),
	      (BindBasis.opNEQ, Basis.opNEQ),
	      (BindBasis.opLTE, Basis.opLTE),
	      (BindBasis.opLT, Basis.opLT),
	      (BindBasis.opADD, Basis.opADD),
	      (BindBasis.opSUB, Basis.opSUB),
	      (BindBasis.opMUL, Basis.opMUL),
	      (BindBasis.opDIV, Basis.opDIV),
	      (BindBasis.opMOD, Basis.opMOD),
	      (BindBasis.opNEG, Basis.opNEG),
	      (BindBasis.opDEREF, Basis.opDEREF),
	      (BindBasis.varArguments, Basis.varArguments),
	      (BindBasis.varChr, Basis.varChr),
	      (BindBasis.varConcat, Basis.varConcat),
	      (BindBasis.varExit, Basis.varExit),
	      (BindBasis.varFail, Basis.varFail),
	      (BindBasis.varNewRef, Basis.varNewRef),
	      (BindBasis.varPrint, Basis.varPrint),
	      (BindBasis.varSize, Basis.varSize),
	      (BindBasis.varSub, Basis.varSub)
            ])

    fun check (errS, prog) = let
          val cxt = C.new errS
          in
            initBasis ();
            chkProg (cxt, prog)
          end

  end
