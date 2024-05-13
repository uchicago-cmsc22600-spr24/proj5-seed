(* id-props.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * This module defines a number of identifier properties that we use to track the
 * mapping from binding-tree identifiers to AST identifiers.
 *)

structure IdProps : sig

      (* type-variable mapping *)
      val tyvar : BindTree.TyVar.t -> TyVar.t

      (* type-constructor mapping *)
      val tyconSet : BindTree.TycId.t * TyCon.t -> unit
      val tycon : BindTree.TycId.t -> TyCon.t

      (* data-constructor mapping *)
      val dconSet : BindTree.ConId.t * DataCon.t -> unit
      val dcon : BindTree.ConId.t -> DataCon.t

      (* variable mapping *)
      val varSet : BindTree.ValId.t * Var.t -> unit
      val var : BindTree.ValId.t -> Var.t

  end = struct

    structure BT = BindTree

    val {getFn = tyvar, ...} = BT.TyVar.newProp TyVar.new

    val {setFn = tyconSet, getFn = tycon, ...} = let
          fun mkTyc (tyc : BT.TycId.t) : TyCon.t =
                raise Fail("IdProps.tycon: must set mapping for " ^ BT.TycId.nameOf tyc)
          in
            BT.TycId.newProp mkTyc
          end

    val {setFn = dconSet, getFn = dcon, ...} = let
          fun mkTyc (dc : BT.ConId.t) : DataCon.t =
                raise Fail("IdProps.dcon: must set mapping for " ^ BT.ConId.nameOf dc)
          in
            BT.ConId.newProp mkTyc
          end

    val {setFn = varSet, getFn = var, ...} = let
          fun mkVar (x : BT.ValId.t) : Var.t =
                raise Fail("IdProps.var: must set mapping for " ^ BT.ValId.nameOf x)
          in
            BT.ValId.newProp mkVar
          end

  end
