(* main.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Driver for the MiniML compiler.
 *)

structure Main : sig

    val main : string * string list -> OS.Process.status

  end = struct

    structure Opts = Options
    structure UU = UnixUtil

    fun err s = TextIO.output (TextIO.stdErr, s)
    fun err1 c =  TextIO.output1 (TextIO.stdErr, c)
    fun errnl s = (err s; err1 #"\n")

    val isSuccess = OS.Process.isSuccess

    (* check for errors and report them if there are any *)
    fun checkForErrors errStrm =
          if Error.anyErrors errStrm
            then raise Error.ERROR
            else ()

    (* verbose printing *)
    val verbose = Opts.verbose
    fun say msg = if !verbose then print(concat("# " :: msg)) else ()

    (* conditionally dump the various IRs to a file *)
    local
      fun dumpIf irName dump flg arg = if !flg
            then (
              say ["dump ", irName, "\n"];
              dump arg)
            else ()
      fun withOutFile output ext (base, ir) = let
            val outS = TextIO.openOut(OS.Path.joinBaseExt{base=base, ext=SOME ext})
            in
              output(outS, ir);
              TextIO.closeOut outS
            end
      fun dumpSimpleAST irName ext flg arg = if !flg
            then if !Options.noSExp
              then (
                say ["print ", irName, "\n"];
                withOutFile PrintSimple.output ext arg)
              else (
                say ["dump ", irName, "\n"];
                DumpSimpleAST.dumpToFile ext arg)
            else ()
      fun dumpCFGAST irName flg arg = if !flg
            then if !Options.noSExp
              then (
                say ["print ", irName, "\n"];
                withOutFile PrintCFG.output "cfg" arg)
              else (
                say ["dump ", irName, "\n"];
                DumpCFG.dumpToFile arg)
            else ()
    in
    val dumpPT = dumpIf "parse tree" DumpParseTree.dumpToFile Options.dumpPT
    val dumpBT = dumpIf "bind tree" DumpBindTree.dumpToFile Options.dumpBT
    val dumpAST = dumpIf "typed AST" DumpAST.dumpToFile Options.dumpAST
    val dumpSimple = dumpSimpleAST "simple AST" "smpl" Options.dumpSimple
    val dumpBadSimple = dumpSimpleAST "bogus simple AST" "err" (ref true)
    val dumpOpt = dumpSimpleAST "simple AST after optimization" "opt" Options.dumpOpt
    val dumpFVS = dumpIf "free vars"
          DumpFreeVars.dumpToFile Options.dumpFVS
    val dumpClosure = dumpSimpleAST "simple AST after closure" "clos" Options.dumpClosure
    val dumpCFG = dumpCFGAST "CFG" Options.dumpCfg
    end (* local *)

    (* check IR invariants for SimpleAST *)
    fun checkSimple (base, msg, chkCensus, prog) = if !Options.checkSimple
          then (
            say ["checking SimpleAST after ", msg, "\n"];
            if CheckSimple.check (msg, chkCensus, prog)
              then (
                dumpBadSimple (base, prog);
                raise Error.ERROR)
              else ())
          else ()

    (* process an input file that is known to exist *)
    fun doFile (errStrm, filename) = let
          val base = OS.Path.base filename
          (* parse the input file *)
          val parseTree = let
                val inS = TextIO.openIn filename
                val _ = say ["parsing ", filename, "\n"]
                val pt = Parser.parse (inS, errStrm)
                in
                  TextIO.closeIn inS;
                  checkForErrors errStrm;
                  valOf pt
                end
          (* output the parse tree *)
          val _ = dumpPT (base, !Opts.withMarks, parseTree)
          (* binding analysis *)
          val _ = say ["binding analysis ", filename, "\n"]
          val bindTree = Binding.analyze (errStrm, parseTree)
          val _ = checkForErrors errStrm
          (* output the binding tree *)
          val _ = dumpBT (base, !Opts.withMarks, bindTree)
          (* type check the program *)
          val _ = say ["type checking ", filename, "\n"]
          val ast = TypeChecker.check (errStrm, bindTree)
          val _ = checkForErrors errStrm
          (* output AST *)
          val _ = dumpAST (base, ast)
          (* convert to SimpleAST *)
          val _ = say ["simplify ", filename, "\n"]
          val simple = Simplify.translate ast
          (* output SimpleAST *)
          val _ = dumpSimple (base, simple)
          (* do a consistency check on the SimpleAST *)
          val _ = checkSimple (base, "simplify", false, simple)
          (* conditionally optimize the Simple AST code *)
          val simple = let
                val simple = if !Options.optimize
                      then (
                        say ["optimizing ", filename, "\n"];
                        SimpleOpt.transform say simple)
                      else (
                        say ["let-floating ", filename, "\n"];
                        LetFloat.transform (
			  CaseContract.transform simple))
                in
                  (* do an optional consistency check on the SimpleAST *)
                  checkSimple (base, "optimization", true, simple);
                  dumpOpt (base, simple);
                  simple
                end
          (* do free variable analysis of the *)
          val _ = say ["free-variable analysis for ", filename, "\n"]
          val () = FreeVarAnalysis.analyze simple
          val _ = dumpFVS (base, simple)
          (* closure convert *)
          val _ = say ["closure conversion for ", filename, "\n"]
          val simple = Closure.transform simple
	  (* let-float the closure converted code again *)
	  val _ = say ["let-floating ", filename, "\n"];
	  val simple = LetFloat.transform simple
          (* output closure-converted Simple AST *)
          val _ = dumpClosure (base, simple)
	  (* convert first-order SimpleAST to CFG *)
          val _ = say ["convert ", filename, " to CFG\n"]
	  val cfg = Convert.translate simple
          (* check CFG invariants *)
          val _ = CheckCFG.check cfg
          (* output CFG *)
          val _ = dumpCFG (base, cfg)
          in
            (* code generation *)
            say ["code generation for ", filename, "\n"];
            CodeGen.gen (base, cfg);
            (* final steps to produce an executable *)
            if isSuccess (UU.run ("llc", [base ^ ".ll", "-o", base ^ ".s"]))
            andalso isSuccess (UU.patchAssembly (base ^ ".s"))
            andalso isSuccess (UU.run ("cc", [base ^ ".s", UU.runtimePath(), "-o", base]))
              then ()
              else raise Error.ERROR
	  end

    (* print the exception stack trace and the return failure *)
    fun handleExn Error.ERROR = OS.Process.failure
      | handleExn exn = (
          err (concat [
              "uncaught exception ", General.exnName exn,
              " [", General.exnMessage exn, "]\n"
            ]);
          List.app
            (fn s => err (concat ["  raised at ", s, "\n"]))
              (SMLofNJ.exnHistory exn);
          OS.Process.failure)

    (* make an error stream for an input file and limit the
     * number of reported errors to 25.
     *)
    fun mkErrStream srcFile = let
	  val errStrm = Error.mkErrStream srcFile
	  in
	    Error.setErrorLimit (errStrm, 25);
	    errStrm
	  end

    fun main (_, opts) = let
          val srcFile = Options.process opts
          in
            (* check that the input srcFile exists *)
            if not (OS.FileSys.access(srcFile, [OS.FileSys.A_READ]))
              then (
                err (concat[
                    "source srcFile \"", srcFile,
                    "\" does not exist or is not readable\n"
                  ]);
                OS.Process.failure)
            (* process the srcFile *)
            else if !Options.dumpTokens
              then let (* scanner test *)
                (* scan the input and print the tokens to <srcFile>.toks *)
                val errStrm = mkErrStream srcFile
                val base = OS.Path.base srcFile
                val outFile = OS.Path.joinBaseExt{base = base, ext = SOME "toks"}
                val inS = TextIO.openIn srcFile
                val outS = TextIO.openOut outFile
                val _ = say ["testing scanner; output to ", outFile, "\n"]
                val sts = Parser.lexer (inS, errStrm, outS)
                in
                  TextIO.closeIn inS; TextIO.closeOut outS;
                  if sts then OS.Process.failure else OS.Process.success
                end
              else let
                (* compile the input *)
                val errStrm = mkErrStream srcFile
                fun finish () = if Error.anyErrors errStrm
                      then (
                        Error.report (TextIO.stdErr, errStrm);
                        OS.Process.failure)
                      else (
                        if Error.anyWarnings errStrm
                          then Error.report (TextIO.stdErr, errStrm)
                          else ();
                        OS.Process.success)
                in
                  (doFile (errStrm, srcFile); finish())
                    handle ex => (ignore (finish()); handleExn ex)
                end
          end (* main *)
            handle ex => handleExn ex

  end
