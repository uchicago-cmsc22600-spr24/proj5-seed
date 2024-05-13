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
 * A context for type checking.  Because we have already resolved variable bindings,
 * we do not need any environment structures in the context.  Instead we just track
 * the current source location and the error stream.
 *)

structure Context : sig

    type t

    (* create a new context initialized to the ML Lite basis environment
     * and an empty span from an error stream
     *)
    val new : Error.err_stream -> t

    (* report an error using the context's error stream and span *)
    val error : t * string list -> unit

    (* return the current span stored in the context *)
    val spanOf : t -> Error.span

    (* `setSpan (span, cxt)` returns a new context that is identical to `cxt`
     * except that its span is `span`.
     *)
    val setSpan : t * Error.span -> t

    (* `withMark (cxt, {span, tree})` takes a context and a marked tree and returns
     * a pair (cxt', tree), where cxt' has the span taken from the marked tree.
     *)
    val withMark : t * 'a Error.mark -> t * 'a

    (* increase the lambda-nesting depth *)
    val incDepth : t -> t

    (* get the current lambda-nesting depth *)
    val depthOf : t -> int

  end = struct

    datatype t = Cxt of {
        errStrm : Error.err_stream,     (* error streem for reporting errors *)
        span : Error.span,              (* the current source-code span *)
        depth : int                     (* the current lambda-nesting depth *)
      }

    fun new errS = Cxt{
            errStrm = errS,
            span = (0, 0),
            depth = 0
          }

    fun error (Cxt{errStrm, span, ...}, msg) = Error.errorAt(errStrm, span, msg)

    fun spanOf (Cxt{span, ...}) = span

    fun setSpan (Cxt{errStrm, depth, ...}, span) =
          Cxt{errStrm=errStrm, span=span, depth=depth}

    fun withMark (cxt, {span, tree}) = (setSpan(cxt, span), tree)

    fun incDepth (Cxt{errStrm, span, depth}) =
          Cxt{errStrm=errStrm, span=span, depth=depth+1}

    fun depthOf (Cxt{depth, ...}) = depth

  end
