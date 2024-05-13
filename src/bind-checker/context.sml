(* context.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Contexts for binding analysis
 *)

structure Context : sig

    (* environments used to track certain kinds of MiniML identifiers.  We expose these
     * types so that the environments can be constructed separately from the context
     * and then merged into the context at a later point in the binding analysis.
     *)
    type tyvar_env = BindTree.tyvar AtomMap.map
    type dcon_env = BindTree.conid AtomMap.map
    type var_env = BindTree.valid AtomMap.map

    (* the contaxt captures both the syntactic and semantic context
     * needed to check the bindings of a MiniML program
     *)
    type t

    (* create a new context initialized to the MiniML basis environment
     * and an empty span from an error stream
     *)
    val new : Error.err_stream -> t

    (* report an error using the context's error stream and span *)
    val error : t * string list -> unit

    (* get the error stream out of the context *)
    val errStrmOf : t -> Error.err_stream

    (* `setSpan (span, cxt)` returns a new context that is identical
     * to `cxt` except that its span is `span`.
     *)
    val setSpan : t * Error.span -> t

    (* return the span of the context *)
    val spanOf : t -> Error.span

    (* set the type-variable environment of a context. The environment's bindings
     * replace the previous type-variable bindings in the context (if any).
     *)
    val setTVEnv : t * tyvar_env -> t

    (* lookup a type variable by name; `NONE` means that it is unbound in the context *)
    val findTyVar : t * Atom.atom -> BindTree.tyvar option

    (* add a type-constructor binding to the context *)
    val bindTyCon : t * Atom.atom * BindTree.tycon -> t

    (* lookup a type by name; `NONE` means that it is unbound in the context *)
    val findTyCon : t * Atom.atom -> BindTree.tycon option

    (* merge a data-constructor-variable environment into a context. The
     * environment's bindings override the context's bindings.
     *)
    val mergeConEnv : t * dcon_env -> t

    (* lookup a data-constructor by name; `NONE` means that it is unbound
     * in the context
     *)
    val findCon : t * Atom.atom -> BindTree.conid option

    (* merge a value-variable environment into a context. The environment's
     * bindings override the context's bindings.
     *)
    val mergeVarEnv : t * var_env -> t

    (* insert a single variable into the context *)
    val bindVar : t * Atom.atom * BindTree.valid -> t

    (* lookup a variable by name; `NONE` means that it is unbound in the context *)
    val findVar : t * Atom.atom -> BindTree.valid option

    (* convenience function for looking up binary and unary operator symbols *)
    val lookupOp : t * Atom.atom -> BindTree.valid

    (* print the environments of a context to stdOut (for debugging) *)
    val dump : t -> unit

  end = struct

    structure BT = BindTree
    structure AMap = AtomMap

    type tyvar_env = BindTree.tyvar AMap.map
    type tycon_env = BindTree.tycon AMap.map
    type dcon_env = BindTree.conid AMap.map
    type var_env = BindTree.valid AMap.map

    datatype t = Cxt of {
        errStrm : Error.err_stream,     (* error stream for reporting errors *)
        span : Error.span,              (* span containing the term being checked *)
        tvEnv : tyvar_env,              (* type-variable environment *)
        tycEnv : tycon_env,             (* type-constructor environment *)
        conEnv : dcon_env,              (* data-constructor environment *)
        varEnv : var_env                (* value-variable environment *)
      }

    fun error (Cxt{errStrm, span, ...}, msg) = Error.errorAt(errStrm, span, msg)

    fun errStrmOf (Cxt{errStrm, ...}) = errStrm

    fun setSpan (Cxt{errStrm, tvEnv, tycEnv, conEnv, varEnv, ...}, span) = Cxt{
            errStrm=errStrm, span=span,
            tvEnv=tvEnv, tycEnv=tycEnv,
            conEnv=conEnv, varEnv=varEnv
          }

    fun spanOf (Cxt{span, ...}) = span

    fun setTVEnv (Cxt stuff, tvEnv) = Cxt{
            errStrm = #errStrm stuff,
            span = #span stuff,
            tvEnv = tvEnv,
            tycEnv = #tycEnv stuff,
            conEnv = #conEnv stuff,
            varEnv = #varEnv stuff
          }

    fun bindTyCon (Cxt stuff, tyc, def) = Cxt{
            errStrm = #errStrm stuff,
            span = #span stuff,
            tvEnv = #tvEnv stuff,
            tycEnv = AMap.insert (#tycEnv stuff, tyc, def),
            conEnv = #conEnv stuff,
            varEnv = #varEnv stuff
          }

    fun mergeConEnv (Cxt stuff, conEnv) = Cxt{
            errStrm = #errStrm stuff,
            span = #span stuff,
            tvEnv = #tvEnv stuff,
            tycEnv = #tycEnv stuff,
            conEnv = AMap.unionWith #2 (#conEnv stuff, conEnv),
            varEnv = #varEnv stuff
          }

    fun mergeVarEnv (Cxt stuff, varEnv) = Cxt{
            errStrm = #errStrm stuff,
            span = #span stuff,
            tvEnv = #tvEnv stuff,
            tycEnv = #tycEnv stuff,
            conEnv = #conEnv stuff,
            varEnv = AMap.unionWith #2 (#varEnv stuff, varEnv)
          }

    fun bindVar (Cxt stuff, x, x') = Cxt{
            errStrm = #errStrm stuff,
            span = #span stuff,
            tvEnv = #tvEnv stuff,
            tycEnv = #tycEnv stuff,
            conEnv = #conEnv stuff,
            varEnv = AMap.insert (#varEnv stuff, x, x')
          }

    fun findTyVar (Cxt{tvEnv, ...}, id) = AMap.find(tvEnv, id)
    fun findTyCon (Cxt{tycEnv, ...}, id) = AMap.find(tycEnv, id)
    fun findCon (Cxt{conEnv, ...}, id) = AMap.find(conEnv, id)
    fun findVar (Cxt{varEnv, ...}, id) = AMap.find(varEnv, id)

    fun lookupOp (cxt, id) = (case findVar (cxt, id)
           of SOME varUse => varUse
            | NONE => raise Fail(concat["unknown operator '", Atom.toString id, "'"])
          (* end case *))

    structure B = BindBasis

    fun new errStrm = let
          fun insTyc (tyc, env) = AMap.insert(env, Atom.atom(BT.TycId.nameOf tyc), tyc)
          fun insCon (dc, env) = AMap.insert(env, Atom.atom(BT.ConId.nameOf dc), dc)
          fun insVar (x, env) = AMap.insert(env, Atom.atom(BT.ValId.nameOf x), x)
          in
            Cxt{
                errStrm = errStrm,
                span = (0, 0),
                tvEnv = AMap.empty,
                tycEnv = List.foldl insTyc AMap.empty [
                    B.tycBool,
                    B.tycInt,
                    B.tycList,
                    B.tycString,
                    B.tycUnit
                  ],
                conEnv = List.foldl insCon AMap.empty [
                    B.conTrue, B.conFalse,
                    B.conNil, B.conCons
                  ],
                varEnv = List.foldl insVar AMap.empty [
                    (* binary operators *)
                    B.opASSIGN,
                    B.opEQL,
                    B.opNEQ,
                    B.opLTE,
                    B.opLT,
                    B.opADD,
                    B.opSUB,
                    B.opMUL,
                    B.opDIV,
                    B.opMOD,
                    (* unary operators *)
                    B.opDEREF,
                    B.opNEG,
                    (* predefined value variables *)
                    B.varArguments,
                    B.varChr,
                    B.varConcat,
                    B.varExit,
                    B.varFail,
                    B.varNewRef,
                    B.varPrint,
                    B.varSize,
                    B.varSub
                  ]
              }
          end (* new *)

    fun dump (Cxt stuff) = let
          fun pr l = print(concat l)
          fun prId id2s (atm, id) = pr ["    ", Atom.toString atm,  " :-> ", id2s id, "\n"]
          fun prEnv name prItem env = if AtomMap.isEmpty env
                then pr ["# ", name, " = { }\n"]
                else (
                  pr ["# ", name, " = {\n"];
                  AtomMap.appi prItem env;
                  pr ["  }\n"])
          in
            print "##### CONTEXT #####\n";
            prEnv "tvEnv" (prId BT.TyVar.toString) (#tvEnv stuff);
            prEnv "tycEnv" (prId BT.TycId.toString) (#tycEnv stuff);
            prEnv "conEnv" (prId BT.ConId.toString) (#conEnv stuff);
            prEnv "varEnv" (prId BT.ValId.toString) (#varEnv stuff);
            print "#####\n"
          end

  end (* Context *)
