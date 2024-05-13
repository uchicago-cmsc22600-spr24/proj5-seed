(* simple-basis.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * This module maps the AST representation of Basis variables to either
 * primitive operators or runtime-system functions.
 *)

structure SimpleBasis : sig

    (* representation of variables *)
    datatype t
      = UserVar                 (* returned for non-basis variables *)
      | PrimOp of Prim.t        (* returned for variables that map to a prim op *)
      | CondOp of PrimCond.t    (* returned for variables that map to a conditional op *)
      | RTFun of Runtime.t      (* returned for variables that map to a runtime-system function *)

    (* map a variable to its SimpleAST representation *)
    val lookup : AST.valid -> t

    (* builtin data constructors *)
    val conTrue  : SimpleDataCon.t
    val conFalse : SimpleDataCon.t
    val conCons  : SimpleDataCon.t
    val conNil   : SimpleDataCon.t

  end = struct

    structure B = Basis
    structure Tbl = Var.Tbl
    structure P = Prim
    structure C = PrimCond
    structure RT = Runtime

    datatype t
      = UserVar
      | PrimOp of P.t
      | CondOp of C.t
      | RTFun of RT.t

    (* the table mapping primitive AST variables to their corresponding
     * SimpleAST representations.  Note that we treat the "arguments"
     * variable separately, since it is bound by the program.
     *)
    val tbl = let
          val tbl = Tbl.mkTable (32, Fail "Simple Basis Tbl")
          val ins = Tbl.insert tbl
          in
            List.app ins [
               (* operators *)
               (B.opEQL,        CondOp C.IntEq),
               (B.opNEQ,        CondOp C.IntNEq),
               (B.opLTE,        CondOp C.IntLte),
               (B.opLT,         CondOp C.IntLt),
               (B.opADD,        PrimOp P.IntAdd),
               (B.opSUB,        PrimOp P.IntSub),
               (B.opMUL,        PrimOp P.IntMul),
               (B.opDIV,        PrimOp P.IntDiv),
               (B.opMOD,        PrimOp P.IntMod),
               (B.opNEG,        PrimOp P.IntNeg),
               (* reference operators *)
               (B.opASSIGN,     PrimOp P.RefAssign),
               (B.opDEREF,      PrimOp P.RefDeref),
               (* predefined variables *)
               (B.varChr,       RTFun RT.funStrChr),
               (B.varConcat,    RTFun RT.funConcat),
               (B.varExit,      RTFun RT.funExit),
               (B.varFail,      RTFun RT.funFail),
               (B.varNewRef,    PrimOp P.RefNew),
               (B.varPrint,     RTFun RT.funPrint),
               (B.varSize,      PrimOp P.StrSize),
               (B.varSub,       PrimOp P.StrSub)
              ];
            tbl
          end

    fun lookup x = if Var.isPrim x
          then Tbl.lookup tbl x
          else UserVar

    local
      structure PTy = PrimType
      structure DC = SimpleDataCon
      fun mkCon (dc, argTys, rep) = DC.new(DataCon.nameOf dc, argTys, rep)
    in
    val conTrue = mkCon(B.conTrue, [], DC.Enum 1)
    val conFalse = mkCon(B.conFalse, [], DC.Enum 0)
    val conCons = mkCon(B.conCons, [PTy.anyTy, PTy.anyTy], DC.Transparent PTy.Boxed)
    val conNil = mkCon(B.conNil, [], DC.Enum 0)
    end

  end
