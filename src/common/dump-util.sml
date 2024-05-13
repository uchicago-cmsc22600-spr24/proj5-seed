(* dump-util.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Utility code for dumping IRs to files in S-expression syntax.
 *)

structure DumpUtil : sig

    (* `dump ext (stem, sexp)` dumps the S-expression `sexp` to the file `stem.ext` *)
    val dump : string -> string * SExp.value -> unit

  end = struct

    fun dump ext (stem, sexp) = let
          val file = OS.Path.joinBaseExt{base = stem, ext = SOME ext}
          val outS = TextIO.openOut file
          val ppStrm = TextIOPP.openOut {dst=outS, wid=120}
          in
            SExpPP.output (ppStrm, sexp);
            TextIOPP.closeStream ppStrm;
            TextIO.closeOut outS
          end

  end
