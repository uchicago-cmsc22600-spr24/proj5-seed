(* chk-ty.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *)

structure ChkTy : sig

    (* convert a binding-tree type to an AST type, while checking the
     * well-formedness of the type expression.
     *)
    val check : Context.t * BindTree.ty -> Type.ty

  end = struct

    structure BT = BindTree
    structure Ty = Type
    structure C = Context

    fun check (cxt, ty) = (case ty
           of BT.MarkTy mark => check (Context.withMark (cxt, mark))
            | BT.VarTy tv => Ty.VarTy(IdProps.tyvar tv)
            | BT.ConTy(tyc, tys) => let
                val tys' = List.map (fn ty => check(cxt, ty)) tys
                val tyc' = IdProps.tycon tyc
                in
                  if (TyCon.arityOf tyc' <> List.length tys')
                    then (
                      C.error (cxt, [
                          "arity mismatch in application of '",
                          BT.TycId.nameOf tyc, "'"
                        ]);
                      Ty.ErrorTy)
                    else Ty.ConTy(tyc', tys')
                end
            | BT.FunTy(ty1, ty2) => Ty.funTy(check(cxt, ty1), check(cxt, ty2))
            | BT.TupleTy tys => Ty.tupleTy(List.map (fn ty => check(cxt, ty)) tys)
          (* end case *))

  end
