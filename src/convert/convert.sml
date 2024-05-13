(* convert.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Conversion from the first-order CFG produced by closure conversion
 * to the CFG IR.
 *)

structure Convert : sig

    val translate : SimpleAST.program -> CFG.program

  end = struct

    structure S = SimpleAST
    structure SV = SimpleVar
    structure VSet = SV.Set
    structure SDC = SimpleDataCon
    structure PTy = PrimType
    structure P = Prim
    structure PC = PrimCond
    structure LV = LiveVarAnalysis

    (* import decision trees for cases *)
    datatype decision_tree = datatype CaseSplit.decision_tree

    (* list of live variables at an expression *)
    fun liveVarListAt e = VSet.toList(LV.liveVarsAt e)

    (********** Function properties **********)

    (* a property to track the entry label of a function *)
    local
      fun mkLabel f = CFGLabel.new (SV.nameOf f)
      val {peekFn, setFn, ...} = SV.newProp mkLabel
    in
    (* assign labels to all of the functions in the program *)
    fun assignLabs (S.E(_, e)) = (case e
           of S.E_LET(_, S.R_EXP e1, e2) => (assignLabs e1; assignLabs e2)
            | S.E_LET(_, _, e) => assignLabs e
            | S.E_FUN(_, f, _, body, e) => (
                setFn (f, mkLabel f);
                assignLabs body;
                assignLabs e)
            | S.E_IF(_, _, e1, e2) => (assignLabs e1; assignLabs e2)
            | S.E_CASE(_, rules) => List.app (assignLabs o #2) rules
            | _ => ()
          (* end case *))
    val setLabel = setFn
    val getLabel = peekFn
    end (* local *)

    (********** Conversion environment **********)

    val bindVar = Env.bind

    fun lookupVar (env, x) = (case getLabel x
           of SOME lab => CFG.CODE lab
            | NONE => Env.lookup (env, x)
          (* end case *))

    (* lookup a list of CFG variables *)
    fun lookupVars (env, xs) = List.map (fn x => lookupVar(env, x)) xs

    (* convert a SimpleAST value to a CFG value *)
    fun cvtVal env v = (case v
           of S.V_VAR x => lookupVar (env, x)
            | S.V_CON dc => let
                val SDC.Enum i = SDC.repOf dc
                in
                  CFG.INT(IntInf.fromInt i)
                end
            | S.V_INT n => CFG.INT n
            | S.V_STR s => CFG.STRING s
          (* end case *))

    (* convert a list of SimpleAST values to CFG values *)
    fun cvtVals (env, vs) = List.map (cvtVal env) vs

    (* convert a list of SimpleAST variables to CFG values *)
    fun cvtVars (env, xs) = List.foldr (fn (x, vs) => lookupVar (env, x)::vs) [] xs

    (* create a new CFG variable for a SimpleAST variable *)
    fun newVar x = CFGVar.new(SV.nameOf x, SV.typeOf x)

    (* bind a variable to a new CFG variable *)
    fun bindNewVar (env, x) = let
          val x' = newVar x
          in
            (x', bindVar(env, x, CFG.VAR x'))
          end

    (* given a list of SimpleAST variables that are the parameters to a fragment,
     * create fresh CFG variables and initialize an environment that maps from the
     * SimpleAST variables to their CFG counterparts.
     *)
    fun mkParams (env, xs) = let
          val xs' = List.map newVar xs
          val env = ListPair.foldl
                (fn (x, x', env) => bindVar(env, x, CFG.VAR x'))
                  (Env.newFrag env) (xs, xs')
          in
            (xs', env)
          end

    (********** Conversion context **********)

    (* this datatype represents the continuation of an expression *)
    datatype continuation
      (* `TAIL ty` represents the tail position of the current function.
       * The type `ty` is the return type of the function.
       *)
      = TAIL of PTy.t
      (* `JOIN(ty, k)` represents a join point for an `IfExp` or `CaseExp`.
       * The type `ty` is the type of value that flows to the join and the
       * function `k` takes an environment and the argument for the join
       * (i.e., the result of a non-tail application or `RetExp`) and returns
       * a jump to the join fragment.
       *)
      | JOIN of PTy.t * (Env.t * CFG.value -> CFG.jump)

    (********** Conversion **********)

    (* convert a first-order SimpleAST function to a CFG function and add
     * it to the list of functions
     *)
    fun cvtFunc (env, (retTy, f, xs, body)) = let
          val isSelfTail = LV.isSelfTailRecursive f
          (* prepare the environment for the function *)
          val env = Env.newFun (env, f, isSelfTail)
          (* convert the function's parameters to CFG variables *)
          val (xs', env) = mkParams (env, xs)
          val SOME f' = getLabel f
          val entryFrag = if isSelfTail
              (* need to create a trivial entry fragment that transfers control to
               * the entry fragment.
               *)
              then let
                (* the label of the header fragment *)
                val hdrLab = Env.getHeaderLable env
                (* the trivial entry fragment *)
                val entryFrag = CFGFrag.new (f', xs',
                      CFG.mkGOTO(hdrLab, List.map CFG.VAR xs'))
                in
                  (* add the header fragment *)
                  expToFrag (env, hdrLab, xs, body, TAIL retTy);
                  entryFrag
                end
              else CFGFrag.new (f', xs', cvtExp (env, body, TAIL retTy))
          in
            Env.addFun (env, CFGFunct.new (retTy, entryFrag :: Env.getFrags env))
          end
(*DEBUG*)handle ex => raise ex

    (* `expToFrag (env, lab, params, body, cont)` translates the expression `body`
     * to a CFG fragment, where `lab` is its label, `params` are the SimpleAST
     * parameters, and `cont` is the continuation for the fragment.  The resulting
     * fragment is added to the list of fragments for the current function.
     *)
    and expToFrag (env, lab, params, body, cont) = let
          val (params', env') = mkParams (env, params)
          val frag = CFGFrag.new(lab, params', cvtExp (env', body, cont))
          in
            Env.addFrag (env, frag)
          end
(*DEBUG*)handle ex => raise ex

    (* `cvtExp (env, exp, cont)` converts a SimpleAST expression to CFG. *)
    and cvtExp (env, exp as S.E(_, e), cont) = (case e
           of S.E_LET(x, S.R_EXP(rhsExp as S.E(_, rhs)), scope) => (case rhs
                 of S.E_LET _ => raise Fail "impossible: nested let"
                  | S.E_FUN _ => raise Fail "impossible: nested fun"
                  | S.E_APPLY(f, vs) => raise Fail "YOUR CODE HERE"
                  | S.E_IF(tst, vs, eThen, eElse) => raise Fail "YOUR CODE HERE"
                  | S.E_CASE(S.V_VAR y, rules) => raise Fail "YOUR CODE HERE"
                  | S.E_CASE _ => raise Fail "invalid case argument"
                  | S.E_RET v => cvtExp (bindVar(env, x, cvtVal env v), scope, cont)
                (* end case *))
            | S.E_LET(x, S.R_PRIM(p, vs), scope) => raise Fail "YOUR CODE HERE"
            | S.E_LET(x, S.R_CALL(cf, vs), scope) => raise Fail "YOUR CODE HERE"
            | S.E_LET(x, S.R_TUPLE[], scope) => raise Fail "YOUR CODE HERE"
            | S.E_LET(x, S.R_TUPLE vs, scope) => raise Fail "YOUR CODE HERE"
            | S.E_LET(x, S.R_SELECT(i, y), scope) => raise Fail "YOUR CODE HERE"
            | S.E_LET(x, S.R_DCON(dc, vs), scope) => raise Fail "YOUR CODE HERE"
            | S.E_FUN(retTy, f, xs, body, e) => (
                (* add `f` to work list *)
                Env.addFunToWL (env, (retTy, f, xs, body));
                cvtExp (env, e, cont))
            | S.E_APPLY(f, vs) => raise Fail "YOUR CODE HERE"
            | S.E_IF(tst, vs, eThen, eElse) => raise Fail "YOUR CODE HERE"
            | S.E_CASE(S.V_VAR x, rules) =>raise Fail "YOUR CODE HERE"
            | S.E_CASE _ => raise Fail "invalid case argument"
            | S.E_RET v => (case cont
                 of TAIL _ => CFG.mkRETURN(cvtVal env v)
                  | JOIN(_, k) => CFG.mkGOTO(k (env, cvtVal env v))
                (* end case *))
          (* end case *))
(*DEBUG*)handle ex => raise ex

    (* the name of the program entry point *)
    val progEntryName = "_mml_entry"

    (* convert the first-order SimpleAST program to CFG *)
    fun translate (prog as S.PROG(vArgs, body)) = let
          (* do live-variable analysis for the program *)
          val () = LV.analyze prog
          (* define the program's entry label *)
          val entryLab = CFGLabel.newExport progEntryName
          val () = setLabel(SV.progName, entryLab)
          (* define the labels for the other functions in the program *)
          val () = assignLabs body
          (* the initial enviroment for processing the top-level expression *)
          val env = Env.new ()
          (* seed the work list with the main function *)
          val () = Env.addFunToWL (env, (PTy.intTy, SV.progName, [vArgs], body))
          (* iterate until the work list is empty *)
          fun iterate () = (case Env.getFunFromWL env
                 of SOME lam => (
                      cvtFunc (env, lam);
                      iterate ())
                  | NONE => ()
                (* end case *))
          val () = iterate ()
          in
            CFG.PROG(Env.getFuns env)
          end

  end
