(* unify.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Destructive unification of type terms.
 *)

structure Unify : sig

    (* destructively unify two types; returns `true` if successful and
     * `false` otherwise.
     *)
    val unify : Type.ty * Type.ty -> bool

  end = struct

    structure Ty = Type
    structure MV = MetaVar

    (* does a meta-variable occur in a type? *)
    fun occursIn (mv, ty) = let
	  fun occurs ty = (case Ty.prune ty
		 of (Ty.VarTy _) => raise Fail "unexpected bound type variable"
		  | (Ty.MetaTy mv') => MV.same(mv, mv')
		  | (Ty.ConTy(_, args)) => List.exists occurs args
                  | Ty.ErrorTy => false
		(* end case *))
	  in
	    occurs ty
	  end

    (* adjust the depth of any non-instantiated meta-variable that is bound
     * deeper than the given depth.
     *)
    fun adjustDepth (ty, depth) = let
	  fun adjust (Ty.MetaTy(Ty.MV{info as ref(Ty.UNIV d), ...})) =
		if (depth < d) then info := Ty.UNIV depth else ()
	    | adjust (Ty.MetaTy(Ty.MV{info=ref(Ty.INST ty), ...})) = adjust ty
	    | adjust (Ty.VarTy _) = raise Fail "unexpected bound type variable"
	    | adjust (Ty.ConTy(_, args)) = List.app adjust args
            | adjust Ty.ErrorTy = ()
	  in
	    adjust ty
	  end

    (* destructively unify two types *)
    fun unify (ty1, ty2) = (case (Ty.prune ty1, Ty.prune ty2)
	   of (Ty.MetaTy mv1, Ty.MetaTy mv2) => (
		if MV.same(mv1, mv2) then ()
		else if MV.isDeeper(mv1, mv2)
		  then MV.instantiate(mv1, ty2)
		  else MV.instantiate(mv2, ty1);
		true)
	    | (Ty.MetaTy mv1, ty2) => unifyWithMV (ty2, mv1)
	    | (ty1, Ty.MetaTy mv2) => unifyWithMV (ty1, mv2)
	    | (Ty.ConTy(tyc1, tys1), Ty.ConTy(tyc2, tys2)) =>
		(TyCon.same(tyc1, tyc2)) andalso ListPair.allEq unify (tys1, tys2)
	    | _ => false
	  (* end case *))

    (* unify a type with an uninstantiated meta-variable *)
    and unifyWithMV (ty, mv as Ty.MV{info=ref(Ty.UNIV d), ...}) =
	  if (occursIn(mv, ty))
	    then false
	    else (adjustDepth(ty, d); MV.instantiate(mv, ty); true)
      | unifyWithMV _ = raise Fail "impossible"

  end
