(* binding.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Binding analysis for MiniML.
 *)

structure Binding : sig

    (* check the bindings in a MiniML parse-tree and return a binding-tree
     * representation that makes the connection between binding and use
     * occurrences of identifiers explicit.
     *)
    val analyze : Error.err_stream * ParseTree.program -> BindTree.program

  end = struct

    structure PT = ParseTree
    structure BT = BindTree
    structure AMap = AtomMap
    structure C = Context

    (* dummy binding-trees that we can use when an error prevents
     * us from constructing an actual tree.
     *)
    val bogusTy = BT.TupleTy[]
    val bogusExp = BT.TupleExp[]
    val bogusPat = BT.WildPat
    val bogusDcl = BT.ValDcl(BT.DoExpBind bogusExp)

    (* The following two helper functions are used to process the mark nodes
     * in the parse tree.
     *
     * `chkWithMark wrap chk (cxt, {span, tree})` applies the `chk` function
     * to `tree` using a context that has been updated with the `span`.  The
     * resulting bind-tree form is then paired with span and wrapped by the
     * bind-tree constructor `wrap`.
     *)
    fun chkWithMark wrap chk (cxt, {span, tree}) =
          wrap {span = span, tree = chk (C.setSpan(cxt, span), tree)}

    (* `chkWithMark'` is similar to `chkWithMark`, except that it handles
     * `chk` functions that return an extended context.
     *)
    fun chkWithMark' wrap chk (cxt, {span, tree}) = let
          val (tree', cxt') = chk (C.setSpan(cxt, span), tree)
          in
            (wrap {span = span, tree = tree'}, cxt')
          end

    fun analyze (errS, prog) = let
          (* report an unbound-identifier error *)
          fun unbound (cxt, kind, id) =
                Error.errorAt(errS, C.spanOf cxt, [
                    "unbound ", kind, " `", Atom.toString id, "`"
                  ])
          (* report a duplicateId identifier error; the second argument specifies
           * the kind of identifier as a string.
           *)
          fun duplicateId (cxt, kind, x) = Error.errorAt (errS, C.spanOf cxt, [
                  "duplicate ", kind, " `", Atom.toString x, "` "
                ])
          (* check a list of bound type variables *)
          fun chkTyVars (cxt, tvs) = let
                fun chkTV (tv, (tvs', tvEnv)) = let
                      val tv' = BT.TyVar.new tv
                      in
                        if AMap.inDomain (tvEnv, tv)
                          then duplicateId (cxt, "type variable", tv)
                          else ();
                        (tv'::tvs', AMap.insert(tvEnv, tv, tv'))
                      end
                val (tvs', tvEnv) = List.foldl chkTV ([], AMap.empty) tvs
                in
                  (List.rev tvs', tvEnv)
                end
          (* analyze a program *)
          fun chkProg (cxt, PT.MarkProg m) = chkWithMark BT.MarkProg chkProg (cxt, m)
            | chkProg (cxt, PT.Prog dcls) = let
                (* process each of the top-level declarations while accumulating their
                 * bindings in the context.
                 *)
                fun chkDcls (cxt, [], dcls') = BT.Prog(List.rev dcls')
                  | chkDcls (cxt, dcl::dcls, dcls') = let
                      val (dcl', cxt) = chkDcl (cxt, dcl)
                      in
                        chkDcls (cxt, dcls, dcl'::dcls')
                      end
                in
                  chkDcls (cxt, dcls, [])
                end
          (* check a top-level declaration *)
          and chkDcl (cxt, PT.MarkDcl m) = chkWithMark' BT.MarkDcl chkDcl (cxt, m)
            | chkDcl (cxt, PT.TypeDcl(tycId, tvs, cons)) = let
                val tycId' = BT.TycId.new tycId
                val cxt' = C.bindTyCon (cxt, tycId, tycId')
                val (tvs', tvEnv) = chkTyVars (cxt', tvs)
                val (cons', conEnv) = chkCons (C.setTVEnv(cxt', tvEnv), cons)
                in
                  (BT.TypeDcl(tycId', tvs', cons'), C.mergeConEnv(cxt', conEnv))
                end
            | chkDcl (cxt, PT.ValDcl vb) = let
                val (vb', cxt') = chkBind (cxt, vb)
                in
                  (BT.ValDcl vb', cxt')
                end
          (* convert a list of parse-tree data constructors to binding trees *)
          and chkCons (cxt, cons) = let
                fun chk ([], cxt, cons', conEnv) = (List.rev cons', conEnv)
                  | chk (con::cons, cxt, cons', conEnv) = let
                      fun chkCon (cxt, PT.MarkCon{span, tree}) = let
                            val (tree', conEnv') = chkCon (C.setSpan(cxt, span), tree)
                            in
                              (BT.MarkCon{span=span, tree=tree'}, conEnv')
                            end
                        | chkCon (cxt, PT.Con(dc, optTy)) = let
                            val dc' = BT.ConId.new dc
                            val optTy' = List.map (fn ty => chkTy (cxt, ty)) optTy
                            in
                              if AMap.inDomain (conEnv, dc)
                                then duplicateId (cxt, "data constructor", dc)
                                else ();
                              (BT.Con(dc', optTy'), AMap.insert(conEnv, dc, dc'))
                            end
                      val (con', conEnv') = chkCon (cxt, con)
                      in
                        chk (cons, cxt, con'::cons', conEnv')
                      end
                val (cons', conEnv) = chk (cons, cxt, [], AMap.empty)
                in
                  (cons', conEnv)
                end
          (* convert types from parse trees to binding trees *)
          and chkTy (cxt, PT.MarkTy m) = chkWithMark BT.MarkTy chkTy (cxt, m)
            | chkTy (cxt, PT.FunTy(ty1, ty2)) =
                BT.FunTy(chkTy(cxt, ty1), chkTy(cxt, ty2))
            | chkTy (cxt, PT.TupleTy tys) =
                BT.TupleTy(List.map (fn ty => chkTy(cxt, ty)) tys)
            | chkTy (cxt, PT.ConTy(tyc, tys)) = let
              (* NOTE: we do *not* check the arity of the type constructor here;
               * that check is left for the typechecking pass.
               *)
                val tys' = List.map (fn ty => chkTy(cxt, ty)) tys
                in
                  case C.findTyCon (cxt, tyc)
                   of SOME tyc' => BT.ConTy(tyc', tys')
                    | NONE => (unbound (cxt, "type constructor", tyc); bogusTy)
                  (* end case *)
                end
            | chkTy (cxt, PT.VarTy tv) = (case C.findTyVar (cxt, tv)
                 of SOME tv' => BT.VarTy tv'
                  | NONE => (unbound (cxt, "type variable", tv); bogusTy)
                (* end case *))
          (* check a value binding *)
          and chkBind (cxt, PT.MarkBind m) = chkWithMark' BT.MarkBind chkBind (cxt, m)
            | chkBind (cxt, PT.FunBind(f, params, e)) = let
                val f' = BT.ValId.new f
                val cxt' = C.bindVar (cxt, f, f')
                fun chkParam (p, (ps', varEnv)) = let
                      val (p', varEnv') = chkPatWithEnv (cxt, p, varEnv)
                      in
                        (p'::ps', varEnv')
                      end
                val (params', varEnv) = List.foldl chkParam ([], AMap.empty) params
                val cxt'' = C.mergeVarEnv (cxt', varEnv)
                in
                  (BT.FunBind(f', List.rev params', chkExp(cxt'', e)), cxt')
                end
            | chkBind (cxt, PT.ValBind(pat, e)) = let
                val (pat', varEnv) = chkPat (cxt, pat)
                in
                  (BT.ValBind(pat', chkExp(cxt, e)), C.mergeVarEnv(cxt, varEnv))
                end
            | chkBind (cxt, PT.DoExpBind e) = (BT.DoExpBind(chkExp(cxt, e)), cxt)
          (* check the bindings of an expression *)
          and chkExp (cxt, PT.MarkExp m) =
                chkWithMark BT.MarkExp chkExp (cxt, m)
            | chkExp (cxt, PT.IfExp(e1, e2, e3)) =
                BT.IfExp(chkExp(cxt, e1), chkExp(cxt, e2), chkExp(cxt, e3))
            | chkExp (cxt, PT.OrElseExp(e1, e2)) =
                BT.OrElseExp(chkExp(cxt, e1), chkExp(cxt, e2))
            | chkExp (cxt, PT.AndAlsoExp(e1, e2)) =
                BT.AndAlsoExp(chkExp(cxt, e1), chkExp(cxt, e2))
            | chkExp (cxt, PT.BinExp(e1, oper, e2)) =
                BT.BinExp(chkExp(cxt, e1), C.lookupOp(cxt, oper), chkExp(cxt, e2))
            | chkExp (cxt, PT.UnExp(oper, e)) =
                BT.UnExp(C.lookupOp(cxt, oper), chkExp(cxt, e))
            | chkExp (cxt, PT.AppExp(e1, e2)) =
                BT.AppExp(chkExp(cxt, e1), chkExp(cxt, e2))
            | chkExp (cxt, PT.TupleExp es) =
                BT.TupleExp(List.map (fn e => chkExp(cxt, e)) es)
            | chkExp (cxt, PT.VarExp x) = (case C.findVar (cxt, x)
                 of SOME varUse => BT.VarExp varUse
                  | NONE => (unbound (cxt, "variable", x); bogusExp)
                (* end case *))
            | chkExp (cxt, PT.ConExp dc) = (case C.findCon(cxt, dc)
                 of SOME dc' => BT.ConExp dc'
                  | NONE => (unbound (cxt, "data constructor", dc); bogusExp)
                (* end case *))
            | chkExp (cxt, PT.IntExp n) = BT.IntExp n
            | chkExp (cxt, PT.StrExp s) = BT.StrExp s
            | chkExp (cxt, PT.CaseExp(e, rules)) = BT.CaseExp(
                chkExp(cxt, e),
                List.map (fn r => chkRule(cxt, r)) rules)
            | chkExp (cxt, PT.BindExp(bnd, e)) = let
                val (bnd', cxt') = chkBind (cxt, bnd)
                in
                  BT.BindExp(bnd', chkExp(cxt', e))
                end
          (* check a case rule *)
          and chkRule (cxt, PT.MarkRule m) = chkWithMark BT.MarkRule chkRule (cxt, m)
            | chkRule (cxt, PT.CaseRule(p, e)) = let
                val (p', varEnv) = chkPat (cxt, p)
                in
                  BT.CaseRule(p', chkExp(C.mergeVarEnv(cxt, varEnv), e))
                end
          (* check a pattern *)
          and chkPatWithEnv (cxt, pat, varEnv) = let
                fun chk (cxt, pat, varEnv) = (case pat
                       of PT.MarkPat{span, tree} => let
                            val (tree', varEnv') =
                                  chk (C.setSpan(cxt, span), tree, varEnv)
                            in
                              (BT.MarkPat{span=span, tree=tree'}, varEnv')
                            end
                        | PT.TuplePat pats => let
                            fun chkOne (p, (ps', varEnv)) = let
                                  val (p', varEnv') = chk (cxt, p, varEnv)
                                  in
                                    (p' :: ps', varEnv')
                                  end
                            val (ps', varEnv') = List.foldl chkOne ([], varEnv) pats
                            in
                              (BT.TuplePat(rev ps'), varEnv')
                            end
                        | PT.ConPat(dc, ps) => let
                            fun chkArgPat (p, (ps', ve)) = let
                                  val (p', ve') = chk (cxt, p, ve)
                                  in
                                    (p'::ps', ve')
                                  end
                            val (ps', ve) = List.foldl chkArgPat ([], varEnv) ps
                            in
                              case C.findCon(cxt, dc)
                               of SOME dc' => (BT.ConPat(dc', List.rev ps'), ve)
                                | NONE => (
                                    unbound (cxt, "data constructor", dc);
                                    (bogusPat, ve))
                              (* end case *)
                            end
                        | PT.VarPat x => let
                            val x' = BT.ValId.new x
                            in
                              if AMap.inDomain (varEnv, x)
                                then duplicateId (cxt, "variable", x)
                                else ();
                              (BT.VarPat x', AMap.insert(varEnv, x, x'))
                            end
                        | PT.WildPat => (BT.WildPat, varEnv)
                      (* end case *))
                in
                  chk (cxt, pat, varEnv)
                end
          and chkPat (cxt, pat) = chkPatWithEnv (cxt, pat, AMap.empty)
          in
            chkProg (C.new errS, prog)
          end (* analyze *)

  end (* Binding *)
