(* chk-pat.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *)

structure ChkPat : sig

    (* type check a pattern with respect to the given type.  Return the AST
     * equivalent pattern.
     *
     * Note: the specified type of the pattern might just be a meta variable
     * (e.g., when the pattern is a function parameter), but when it has more
     * information we can check that against the structure of the pattern.
     *)
    val check : Context.t * Type.ty * BindTree.pat -> AST.pat

  end = struct

    structure BT = BindTree
    structure C = Context
    structure Ty = Type
    structure TU = TypeUtil
    structure U = Unify

    (* placeholder pattern for when there is an error *)
    val bogusPat = AST.TuplePat[]

    fun check (cxt, ty, BT.MarkPat m) : AST.pat = let
          val (cxt, pat) = C.withMark (cxt, m)
          in
            check (cxt, ty, pat)
          end
      | check (cxt, ty, BT.VarPat x) = let
          val x' = Var.new(x, ty)
          in
            IdProps.varSet (x, x');
            AST.VarPat x'
          end
      | check (cxt, ty, BT.ConPat(dc, ps)) = let
          val dc' = IdProps.dcon dc
          val nArgs = List.length ps
          in
            if (nArgs <> DataCon.arityOf dc')
              then (
                C.error(cxt, ["data-constructor arity mismatch"]);
                (* to avoid down-stream errors with any variables bound in the pattern,
                 * we check the argument patterns too.
                 *)
                ignore (
                  List.map (fn p => check(cxt, Ty.metaVarTy (C.depthOf cxt), p)) ps);
                bogusPat)
              else let
                val (tvs, argTys, resTy) = DataCon.tySigOf dc'
                val resTy'::argTys' =
                      TU.instantiateTys (tvs, resTy::argTys, C.depthOf cxt)
                in
                  if not (U.unify (ty, resTy'))
                    then (
                      C.error (cxt, ["type mismatch in constructor pattern"]);
                      bogusPat)
                    else let
                      val ps' = ListPair.map
                            (fn (ty, p) => check(cxt, ty, p))
                              (argTys', ps)
                      in
                        AST.ConPat(dc', ps')
                      end
                end
(*DEBUG*)handle ex => raise ex
          end
      | check (cxt, ty, BT.TuplePat[]) = (
          if not(U.unify(ty, Basis.tyUnit))
            then C.error(cxt, ["type mismatch"])
            else ();
          AST.TuplePat[])
      | check (cxt, ty, BT.TuplePat ps) = let
          val tys = List.map (fn _ => Ty.metaVarTy (C.depthOf cxt)) ps
          in
            if not(U.unify(ty, Ty.tupleTy tys))
              then (
                C.error(cxt, ["type mismatch"]);
                bogusPat)
              else let
                val ps' = ListPair.map (fn (ty, p) => check (cxt, ty, p)) (tys, ps)
                in
                  AST.TuplePat ps'
                end
          end
      | check (cxt, ty, BT.WildPat) = AST.VarPat(Var.wild ty)

  end
