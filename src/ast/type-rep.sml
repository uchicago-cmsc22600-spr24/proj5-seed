(* type.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * This module collects together all of the type declarations related
 * to representing MiniML types.  The exported modules for dealing with
 * types are TyVar, MetaVar, TyCon, and Type, which are defined in other
 * files,
 *)

structure TypeRep =
  struct

    (* bound type variables *)
    type tyvar = TyVar.t

    (* types *)
    datatype ty
      = VarTy of tyvar		        (* bound type variable *)
      | MetaTy of meta_var	        (* unification meta variable *)
      | ConTy of tycon * ty list        (* type constructor application; includes function
                                         * and tuple types.
                                         *)
      | ErrorTy			        (* unknown type used to avoid cascading errors *)

    (* unification meta variables *)
    and meta_var = MV of {
        id : Stamp.t,                   (* unique ID *)
        info : meta_info ref            (* either the instantiation of the variable or
                                         * meta-data about the variable.
                                         *)
      }

    and meta_info
      = UNIV of int                     (* variable introduced at given depth *)
      | INST of ty                      (* substitution instance *)

    (* type schemes *)
    and scheme = TyScm of tyvar list * ty

    (* a type constructor (primitive or introduced by a `type` declaration) *)
    and tycon
      = PrimTyc of {            (* built-in type constructors *)
            name : string,        (* the type's name *)
            id : Stamp.t,         (* unique stamp *)
            params : tyvar list   (* bound type parameters *)
          }
      | FunTyc                  (* '->' type constructor *)
      | TupleTyc of int         (* tuple type constructor; integer is arity (!= 1) *)
      | UserTyc of usr_tyc      (* user-declared type *)

    (* a data constructor *)
    and dcon = DCon of {        (* data constructor *)
        name : string,          (* constructor name *)
        id : Stamp.t,           (* unique identifier *)
        argTys : ty list,       (* argument types; `[]` for nullary constructors *)
        dty : usr_tyc           (* the type constructor that this dcon belongs to *)
      }

    withtype usr_tyc = {
          name : string,        (* the type's name *)
          id : Stamp.t,         (* unique stamp *)
          params : tyvar list,  (* bound type parameters *)
          cons : dcon list ref  (* list of data constructors *)
        }


    fun prune (MetaTy(MV{info = ref(INST ty), ...})) = prune ty
      | prune ty = ty

  end
