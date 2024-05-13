(* basis.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Type constructors, data constructors, and variables defined in the MiniML Basis.
 *)

structure Basis : sig

    (* predefined type constructors *)
    val tycBool : TyCon.t
    val tycInt : TyCon.t
    val tycList : TyCon.t
    val tycRef : TyCon.t
    val tycString : TyCon.t
    val tycUnit : TyCon.t

    (* predefined types *)
    val tyBool : Type.ty
    val tyInt : Type.ty
    val tyList : Type.ty -> Type.ty
    val tyRef : Type.ty -> Type.ty
    val tyString : Type.ty
    val tyUnit : Type.ty

    (* pre-defined data constructors *)
    val conTrue : DataCon.t
    val conFalse : DataCon.t
    val conCons : DataCon.t
    val conNil : DataCon.t

    (* operators *)
    val opASSIGN : Var.t        (* assignment *)
    val opEQL : Var.t           (* integer equality relation *)
    val opNEQ : Var.t           (* integer inequality relation *)
    val opLTE : Var.t           (* integer less-than-or-equal relation *)
    val opLT : Var.t            (* integer less-than relation *)
    val opADD : Var.t           (* integer addition *)
    val opSUB : Var.t           (* integer subtraction *)
    val opMUL : Var.t           (* integer multiplication *)
    val opDIV : Var.t           (* integer division *)
    val opMOD : Var.t           (* integer remainder *)
    val opNEG : Var.t           (* unary integer negation *)
    val opDEREF : Var.t         (* dereference *)

    (* predefined variables *)
    val varArguments : Var.t
    val varChr : Var.t
    val varConcat : Var.t
    val varExit : Var.t
    val varFail : Var.t
    val varNewRef : Var.t
    val varPrint : Var.t
    val varSize : Var.t
    val varSub : Var.t

  end = struct

    structure BB = BindBasis
    structure BT = BindTree
    structure Ty = Type

    (* helper for creating polymorphic type schemes *)
    fun tyScheme mk = let
          val tv = TyVar.new(BT.TyVar.new(Atom.atom "t"))
          in
            Ty.TyScm([tv], mk (Ty.VarTy tv))
          end
    (* monomorphic type scheme *)
    fun monoScheme ty = Ty.TyScm([], ty)

    (* type variables used in the definition of the List and Ref types. *)
    val listTyParam  = TyVar.new(BT.TyVar.new(Atom.atom "t"))
    val listTyParam' = Ty.VarTy listTyParam
    val refTyParam = TyVar.new(BT.TyVar.new(Atom.atom "t"))

    (* predefined type constructors *)
    val tycBool = TyCon.new(BB.tycBool, [])
    val tycInt = Type.intTyc
    val tycList = TyCon.new(BB.tycList, [listTyParam])
    val tycRef = TyCon.newPrim(BB.tycRef, [refTyParam])
    val tycString = Type.stringTyc
    val tycUnit = Type.unitTyc

    (* predefined types *)
    val tyBool = Ty.ConTy(tycBool, [])
    val tyInt = Ty.ConTy(tycInt, [])
    fun tyList ty = Ty.ConTy(tycList, [ty])
    fun tyRef ty = Ty.ConTy(tycRef, [ty])
    val tyString = Ty.ConTy(tycString, [])
    val tyUnit = Ty.ConTy(tycUnit, [])

  (* pre-defined data constructors *)
    val conTrue = DataCon.new (tycBool, BB.conTrue, [])
    val conFalse = DataCon.new (tycBool, BB.conFalse, [])
    val conCons = DataCon.new (
          tycList,
          BB.conCons,
          [listTyParam', tyList listTyParam'])
    val conNil = DataCon.new (tycList, BB.conNil, [])

    (* finish predefined data-type constructors *)
    val _ = (TyCon.finish tycBool; TyCon.finish tycList)

    (* operators *)
    local
      fun pairTy (ty1, ty2) = Ty.tupleTy[ty1, ty2]
      fun binOp (id, ty1, ty2, ty3) =
            Var.prim (id, monoScheme(Ty.funTy(pairTy(ty1, ty2), ty3)))
    in
    val opASSIGN = Var.prim (
          BB.opASSIGN,
          tyScheme(fn t => Ty.funTy(pairTy(tyRef t, t), tyUnit)))
    val opEQL = binOp (BB.opEQL, tyInt, tyInt, tyBool)
    val opNEQ = binOp (BB.opNEQ, tyInt, tyInt, tyBool)
    val opLTE = binOp (BB.opLTE, tyInt, tyInt, tyBool)
    val opLT = binOp (BB.opLT, tyInt, tyInt, tyBool)
    val opADD = binOp (BB.opADD, tyInt, tyInt, tyInt)
    val opSUB = binOp (BB.opSUB, tyInt, tyInt, tyInt)
    val opMUL = binOp (BB.opMUL, tyInt, tyInt, tyInt)
    val opDIV = binOp (BB.opDIV, tyInt, tyInt, tyInt)
    val opMOD = binOp (BB.opMOD, tyInt, tyInt, tyInt)
    val opNEG = Var.prim (BB.opNEG, monoScheme(Ty.funTy(tyInt, tyInt)))
    val opDEREF = Var.prim (BB.opDEREF, tyScheme(fn t => Ty.funTy(tyRef t, t)))
    end (* local *)

    (* predefined variables *)
    local
      fun prim (id, ty) = Var.prim(id, monoScheme ty)
      fun polyPrim (id, mkTy) = Var.prim(id, tyScheme mkTy)
    in
    val varArguments = Var.new (BB.varArguments, tyList tyString)
    val varChr = prim (BB.varChr, Ty.funTy(tyInt, tyString))
    val varConcat = prim (BB.varConcat, Ty.funTy(Ty.tupleTy[tyString, tyString], tyString))
    val varExit = polyPrim (BB.varExit, fn t => Ty.funTy(tyInt, t))
    val varFail = polyPrim (BB.varFail, fn t => Ty.funTy(tyString, t))
    val varNewRef = polyPrim (BB.varNewRef, fn t => Ty.funTy(t, tyRef t))
    val varPrint = prim (BB.varPrint, Ty.funTy(tyString, tyUnit))
    val varSize = prim (BB.varSize, Ty.funTy(tyString, tyInt))
    val varSub = prim (BB.varSub, Ty.funTy(Ty.tupleTy[tyString, tyInt], tyInt))
    end (* local *)

  end
