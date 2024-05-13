(* parser.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Combine the generated parser and lexer to implement the parser.
 *)

structure Parser : sig

    (* parse a MiniML program from an input stream using the given error stream to record errors *)
    val parse : TextIO.instream * Error.err_stream -> ParseTree.program option

    (* `lexer (inStrm, errStrm, outStrm)` tests the scanner by printing tokens
     * from `inStrm` to the given output stream.  Any lexer errors are reported
     * in the output stream after the input is tokenized.  Returns `true` if there
     * were errors or warnings reported, and `false` otherwise.
     *)
    val lexer : TextIO.instream * Error.err_stream * TextIO.outstream -> bool

  end = struct

    structure Tok = MMLTokens

    (* glue together the lexer and parser *)
    structure MMLParser = MMLParseFn(MMLLex)

    (* error function for lexers *)
    fun lexErr errStrm (span, msg) = Error.errorAt(errStrm, span, msg)

    (* to get ADD and DEL constructors *)
    datatype add_or_delete = datatype AntlrRepair.add_or_delete

    (* map tokens to strings *)
    fun tokToString ADD (Tok.LID _) = "<lc-identifier>"
      | tokToString DEL (Tok.LID x) = Atom.toString x
      | tokToString ADD (Tok.UID _) = "<uc-identifier>"
      | tokToString DEL (Tok.UID x) = Atom.toString x
      | tokToString ADD (Tok.NUMBER _) = "<number>"
      | tokToString DEL (Tok.NUMBER i) = IntInf.toString i
      | tokToString ADD (Tok.STRING _) = "<string>"
      | tokToString DEL (Tok.STRING s) = concat["\"", String.toString s, "\""]
      | tokToString _ tok = Tok.toString tok

    (* error function for parsers *)
    val parseErr = Error.parseError tokToString

    (* parse a file, returning a parse tree *)
    fun parse (inStrm, errStrm) = let
          fun get () = TextIO.input inStrm
          val lexer = MMLLex.lex (Error.sourceMap errStrm) (lexErr errStrm)
          val (yield, _, errs) = MMLParser.parse lexer (MMLLex.streamify get)
          in
            List.app (parseErr errStrm) errs;
            yield
          end

    local
      fun toString (Tok.LID x) = "LID " ^ Atom.toString x
        | toString (Tok.UID x) = "UID " ^ Atom.toString x
        | toString (Tok.NUMBER i) = "NUMBER " ^ IntInf.toString i
        | toString (Tok.STRING s) = concat["STRING \"", String.toString s, "\""]
        | toString tok = Tok.toString tok
    in
    fun lexer (inStrm, errStrm, outStrm) = let
          val lex = MMLLex.lex (Error.sourceMap errStrm) (lexErr errStrm)
          (* finish up by reporting the errors (if any) *)
          fun finish () = if Error.anyWarnings errStrm orelse Error.anyErrors errStrm
                then (
                  TextIO.output(outStrm, "===== ERRORS =====\n");
                  Error.report (outStrm, errStrm);
                  true)
                else false
          fun get strm = let
                val (tok, span, strm) = lex strm
                val loc = Error.location (errStrm, span)
                in
                  TextIO.output(outStrm, concat[
                      StringCvt.padRight #" " 23 (toString tok), " ",
                      Error.locToString loc, "\n"
                    ]);
                  case tok
                   of Tok.EOF => finish ()
                    | _ => get strm
                  (* end case *)
                end
          in
            get (MMLLex.streamify (fn () => TextIO.input inStrm))
               handle ex => (
                ignore (finish ());
                TextIO.output(outStrm, concat["***** ", General.exnName ex, " raised\n"]);
                true)
          end
    end (* local *)

  end
