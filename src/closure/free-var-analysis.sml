(* free-var-analysis.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * An analysis pass that computes the free variables of functions.
 *)

structure FreeVarAnalysis : sig

    (* Analyze a SimpleAST program to compute the free variables of functions
     * and the live variables at splits (if-then-else expressions) and joins
     * (let expressions with rhs expressions).
     *)
    val analyze : SimpleAST.program -> unit

    (* return the free variables of a function; the result is only valid after the
     * `analyze` function has been run.
     *)
    val freeVarsOf : SimpleVar.t -> SimpleVar.t list

    (* is the free-variables set of a function empty *)
    val hasNoFreeVars : SimpleVar.t -> bool

  end = struct

    structure S = SimpleAST
    structure V = SimpleVar
    structure Set = V.Set

    (* variable property to record the free variables of functions *)
    local
      val {getFn, setFn, ...} = V.newProp (fn _ => Set.empty)
    in
    val getFreeVars = getFn
    val setFreeVars = setFn
    val freeVarsOf = Set.toList o getFn
    fun hasNoFreeVars x = Set.isEmpty(getFreeVars x)
    end (* local *)

    (* compute the free variables of the functions in the program. *)
    fun freeVars e = let
          fun fvOfVal (S.V_VAR x, fvs) = Set.add(fvs, x)
            | fvOfVal (_, fvs) = fvs
          fun fvsOfVals (vs, fvs) = List.foldl fvOfVal fvs vs
          fun analFB (f, xs, body) = let
                val fvsOfF = Set.subtractList(anal (body, Set.empty), f::xs)
                in
                  (* record f's free variables *)
                  setFreeVars (f, fvsOfF);
(*
print(concat["freeVars(", V.toString f, ") = {",
String.concatWithMap "," V.toString (Set.toList fvsOfF), "}\n"]);
*)
                  fvsOfF
                end
          (* analyze an expression for its free variables *)
          and anal (S.E(ppt, e), fvs) = (case e
                 of S.E_LET(x, rhs, e) => let
                      val fvs = analRHS(rhs, anal (e, fvs))
                      in
                        Set.subtract (fvs, x)
                      end
                  | S.E_FUN(_, f, params, body, e) => let
                      val fvsOfF = analFB (f, params, body)
                      in
                        Set.subtract(anal (e, Set.union(fvs, fvsOfF)), f)
                      end
                  | S.E_APPLY(f, vs) => Set.add(fvsOfVals (vs, fvs), f)
                  | S.E_IF(_, vs, e1, e2) => anal (e2, anal (e1, fvsOfVals (vs, fvs)))
                  | S.E_CASE(v, rules) => List.foldl analRule (fvOfVal (v, fvs)) rules
                  | S.E_RET v => fvOfVal (v, fvs)
                (* end case *))
          (* analyze a right-hand-side for its free variables *)
          and analRHS (rhs, fvs) = (case rhs
                 of S.R_EXP e => anal (e, fvs)
                  | S.R_PRIM(_, vs) => fvsOfVals (vs, fvs)
                  | S.R_CALL(_, vs) => fvsOfVals (vs, fvs)
                  | S.R_TUPLE vs => fvsOfVals (vs, fvs)
                  | S.R_SELECT(_, x) => Set.add(fvs, x)
                  | S.R_DCON(_, vs) => fvsOfVals (vs, fvs)
                (* end case *))
          (* analyze a case rule for its free variables *)
          and analRule ((p, e), fvs) = let
                val fvs = anal (e, fvs)
                in
                  case p
                   of S.P_DCON(_, xs) => Set.subtractList(fvs, xs)
                    | S.P_VAR x => Set.subtract(fvs, x)
                  (* end case *)
                end
          (* top-level decls: we skip any code that is not in a function *)
          fun walk (S.E(_, e)) = (case e
                 of S.E_LET(_, S.R_EXP e1, e2) => (walk e1; walk e2)
                  | S.E_LET(_, _, e) => walk e
                  | S.E_FUN(_, f, xs, body, e) => (ignore (analFB(f, xs, body)); walk e)
                  | S.E_IF(_, _, e1, e2) => (walk e1; walk e2)
                  | S.E_CASE(_, rules) => List.app (fn (_, e) => walk e) rules
                  | _ => ()
                (* end case *))
          in
            walk e
          end

    (* after we have computed the free variables in the body of a top-level
     * function, we remove any references to known functions with empty
     * closures.  This process is implemented as fixed-point computation
     * over the bound functions in the program.
     *)
    fun pruneFVS e = let
          (* compute a set of all of the functions in the program *)
          val allFns = let
                fun accum (S.E(_, e), fns) = (case e
                      of S.E_LET(_, S.R_EXP e1, e2) => accum (e2, accum(e1, fns))
                       | S.E_LET(_, _, e) => accum (e, fns)
                       | S.E_FUN(_, f, _, body, e) => accum (e, accum(body, f::fns))
                       | S.E_APPLY(f, vs) => fns
                       | S.E_IF(_, vs, e1, e2) => accum (e2, accum(e1, fns))
                       | S.E_CASE(v, rules) =>
                           List.foldl (fn ((_, e), fns) => accum(e, fns)) fns rules
                       | S.E_RET v => fns
                     (* end case *))
                in
                  Set.fromList (accum (e, []))
                end
          (* test to see if a free variable should be kept in the set of free variables.
           * The conditions for removal are that the variable is not a fun-bound
           * variable, or not a known function, or has a non-empty closure.
           *)
          fun keep x = not(Set.member(allFns, x))
                orelse (V.useCntOf x > Census.appCntOf x)
                orelse not(hasNoFreeVars x)
          val anyChange = ref false
          fun updateFVs f = let
                val fvs = Set.filter keep (getFreeVars f)
                in
                  setFreeVars (f, fvs);
                  if Set.isEmpty fvs
                    then (anyChange := true; false)
                    else true
                end
          fun lp wlist = let
                  val () = anyChange := false
                  val wlist' = List.filter updateFVs wlist
                  in
                    if !anyChange then lp wlist' else ()
                  end
          in
            lp (Set.foldl (fn (x, wl) => if keep x then x::wl else wl) [] allFns)
          end

    (* free vars analysis *)
    fun analyze (S.PROG(_, e)) = (freeVars e; pruneFVS e)

  end
