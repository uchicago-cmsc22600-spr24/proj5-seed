(* check-simple.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *)

structure CheckSimple : sig

    (* `check (msg, chkCensus, prog)` checks the SimpleAST program for a number
     * of invariants, such as that every variable is used has a binding, variables
     * do not have multiple bindings, and that primitive operations are applied to
     * the correct number of arguments. If the `chkCensus` flag is true, then it
     * also checks that census counts are correct. It reports error to the stdErr
     * stream and will return `true` if there were errors and `false` otherwise.
     *)
    val check : string * bool * SimpleAST.program -> bool

  end = struct

    structure S = SimpleAST
    structure V = SimpleVar
    structure Set = V.Set
    structure Tbl = V.Tbl

    (* get a counter from a counter table (allocate one if necessary) *)
    fun getCntr (tbl : int ref Tbl.hash_table) x = (case Tbl.find tbl x
           of NONE => let
                val cntr = ref 0
                in
                  Tbl.insert tbl (x, cntr);
                  cntr
                end
            | SOME cntr => cntr
          (* end case *))

    fun val2s (S.V_VAR x) = V.toString x
      | val2s (S.V_CON dc) = SimpleDataCon.toString dc
      | val2s (S.V_INT n) = if (n < 0) then "-" ^ IntInf.toString(~n) else IntInf.toString n
      | val2s (S.V_STR s) = concat["\"", String.toString s, "\""]

    (* some context helpers *)
    fun letCxt x = concat["`let ", V.toString x, " = ...`"]
    fun funCxt f = concat["`fun ", V.toString f, " ...`"]
    fun appCxt (f, vs) = concat[
            "`", V.toString f, " (", String.concatWithMap "," val2s vs, ")`"
          ]
    fun primCxt (p, vs) = concat[
            "`", Prim.toString p, " (", String.concatWithMap "," val2s vs, ")`"
          ]
    fun callCxt (cf, vs) = concat[
            "`", Runtime.nameOf cf, " (", String.concatWithMap "," val2s vs, ")`"
          ]
    fun conappCxt (dc, v) = concat["`", SimpleDataCon.toString dc, "(", val2s v, ")`"]
    fun selectCxt (i, x) = concat["`", "#", Int.toString i, "(", V.toString x, ")`"]
    fun ifCxt (tst, vs) = concat[
            "`if ", PrimCond.toString tst, " (", String.concatWithMap "," val2s vs, ")` ...`"
          ]
    fun conapppatCxt (dc, x) = concat["`", SimpleDataCon.toString dc, "(", V.toString x, ")`"]

    fun prErr msg = TextIO.output(TextIO.stdErr, concat msg)

    fun error (cxt, msg) = prErr ("** [" :: cxt :: "]: " :: msg @ ["\n"])

    fun check (phase, chkCensus, S.PROG(vArgs, e)) = let
          val anyErrors = ref false
          fun err (cxt, msg) = (
                if not (!anyErrors)
                  then (
                    prErr ["***** Errors detected in SimpleAST after ", phase, " *****\n"];
                    anyErrors := true)
                  else ();
                error (cxt, msg))
          (* global set of bound variables *)
          val isBound = ref(Set.empty)
          (* check that a variable has not been previoously bound in the program and add it
           * to the set of variables that are in scope.
           *)
          fun chkVarBnd cxt (x, bnd) = (
                if Set.member(!isBound, x)
                  then error (cxt, ["multiple bindings for '", V.toString x, "'"])
                  else isBound := Set.add(!isBound, x);
                Set.add(bnd, x))
          (* shadow use counts *)
          val useCnt : int ref Tbl.hash_table = Tbl.mkTable(64, Fail "use table")
          val getUseCntr = getCntr useCnt
          fun incUseCnt x = let val cntr = getUseCntr x in cntr := !cntr + 1 end
          (* check that the computed use count matches the count
           * we computed walking the scope of the variable.
           *)
          fun chkUseCnt cxt x = if chkCensus andalso V.useCntOf x <> !(getUseCntr x)
                then err (cxt, [
                    "`", V.toString x, "` has incorrect use count ",
                    Int.toString(V.useCntOf x), "; expected ", Int.toString(!(getUseCntr x))
                  ])
                else ()
          (* shadow application counts *)
          val appCnt : int ref Tbl.hash_table = Tbl.mkTable(64, Fail "app table")
          val getAppCntr = getCntr appCnt
          fun incAppCnt f = let val cntr = getAppCntr f in cntr := !cntr + 1 end
          (* check that the computed application count matches the count
           * we computed walking the scope of the function.
           *)
          fun chkAppCnt cxt f = if chkCensus andalso Census.appCntOf f <> !(getAppCntr f)
                then err (cxt, [
                    "`", V.toString f, "` has incorrect application count ",
                    Int.toString(Census.appCntOf f), "; expected ",
                    Int.toString(!(getUseCntr f))
                  ])
                else ()
          (* check that a variable use occurs in its scope and bump the use counter *)
          fun chkVarUse (bnd, cxt) x = (
                  if not (Set.member(bnd, x))
                    then err (cxt, ["unbound use occurrence of `", V.toString x, "`"])
                    else ();
                  incUseCnt x)
          fun chkValue (bnd, cxt) (S.V_VAR x) = chkVarUse (bnd, cxt) x
            | chkValue _ _ = ()
          fun chkValues (bnd, cxt, vs) = List.app (chkValue (bnd, cxt)) vs
          fun chkExp (bnd, S.E(_, e)) = (case e
                 of S.E_LET(x, rhs, e) => let
                      val bnd' = chkVarBnd "let binding" (x, bnd)
                      val cxt = letCxt x
                      in
                        chkRHS (cxt, bnd, rhs);
                        chkExp (bnd', e);
                        chkUseCnt cxt x
                      end
                  | S.E_FUN(_, f, xs, body, e) => let
                      val bnd' = chkVarBnd "fun binding" (f, bnd)
                      val cxt = funCxt f
                      val bnd'' = List.foldl (chkVarBnd cxt) bnd' xs
                      in
                        chkExp (bnd'', body);
                        List.app (chkUseCnt cxt) xs;
                        chkExp (bnd', e);
                        chkUseCnt cxt f;
                        chkAppCnt cxt f
                      end
                  | S.E_APPLY(f, vs) => (
                      chkVarUse (bnd, appCxt (f, vs)) f;
                      chkValues (bnd, appCxt (f, vs), vs);
                      incAppCnt f)
                  | S.E_IF(tst, vs, e1, e2) => (
                      if PrimCond.arityOf tst <> length vs
                        then err(ifCxt (tst, vs), ["arity mismatch"])
                        else ();
                      List.app (chkValue (bnd, ifCxt (tst, vs))) vs;
                      chkExp (bnd, e1);
                      chkExp (bnd, e2))
                  | S.E_CASE(v, rules) => (
                      chkValue (bnd, "case argument") v;
                      List.app (chkRule bnd) rules)
                  | S.E_RET v => chkValue (bnd, "return") v
                (* end case *))
          and chkRHS (lhsCxt, bnd, rhs) = (case rhs
                 of S.R_EXP e => chkExp (bnd, e)
                  | S.R_PRIM(p, vs) => (
                      if Prim.arityOf p <> length vs
                        then err(primCxt (p, vs), ["arity mismatch"])
                        else ();
                      chkValues (bnd, primCxt (p, vs), vs))
                  | S.R_CALL(cf, vs) => (
                      if Runtime.arityOf cf <> length vs
                        then err(callCxt (cf, vs), ["arity mismatch"])
                        else ();
                      chkValues (bnd, callCxt (cf, vs), vs))
                  | S.R_TUPLE vs => chkValues (bnd, "tuple", vs)
                  | S.R_SELECT(i, x) =>
                      chkVarUse (bnd, selectCxt(i, x)) x
                  | S.R_DCON(dc, vs) =>
                      List.app (fn v => chkValue (bnd, conappCxt(dc, v)) v) vs
                (* end case *))
          and chkRule bnd (pat, exp) = chkExp (chkPat (bnd, pat), exp)
          and chkPat (bnd, pat) = (case pat
                 of S.P_DCON(dc, xs) => List.foldl
                      (fn (x, bnd) => chkVarBnd (conapppatCxt (dc, x)) (x, bnd))
                        bnd xs
                  | S.P_VAR x => chkVarBnd "var pat" (x, bnd)
                (* end case *))
          val bnd = chkVarBnd "program" (vArgs, Set.empty)
          in
            chkExp (bnd, e);
            chkUseCnt "program" vArgs;
            !anyErrors
          end

  end
