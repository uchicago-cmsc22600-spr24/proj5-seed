(* unix-util.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Support for that last few stages of compilation using the LLVM toolchain
 *)

structure UnixUtil : sig

    (* run the specified Unix command with the given arguments.  The command must be
     * in the PATH.  If the command is not found or fails, an error message is printed
     * to stderr and OS.Process.failure is returned as the status.  If the command
     * runs successfully, then OS.Process.success is returned.
     *)
    val run : string * string list -> OS.Process.status

    (* add a global declaration for the `__LLVM_StackMaps` symbol to the specified
     * assembly file.
     *)
    val patchAssembly : string -> OS.Process.status

    (* get the path to the runtime system.  We do this by assuming that if the
     * compiler command is "<path>/bin/mlc.sh", then the library will be in
     * "<path>/src/runtime/mml-rt.o".  If the runtime object file does not exist,
     * then we print an error message and raise the Error.ERROR exception.
     *)
    val runtimePath : unit -> string

  end = struct

    structure UP = UnixPath
    structure FSys = OS.FileSys
    structure Proc = OS.Process
    structure Path = OS.Path

    fun err msg = (
          TextIO.output(TextIO.stdErr, String.concat msg);
          OS.Process.failure)

    fun run (cmd, args) = (case UP.findFile (UP.getPath(), [UP.A_EXEC, UP.A_READ]) cmd
           of SOME cmd => let
                val cmdLn = String.concatWith " " (cmd :: args)
                val sts = Proc.system cmdLn
                in
                  if Proc.isSuccess sts
                    then sts
                    else err ["'", cmdLn, "' failed\n"]
                end
            | NONE => err ["'", cmd, "' not found in $PATH\n"]
          (* end case *))

    (* a little wrapper to manage resources *)
    fun tryFinally (arg, f, final) =
          (f arg ; final arg) handle ex => (final arg; raise ex)

    (* a dummy __LLVM_StackMaps table for when there are no function calls in
     * the generated LLVM IR.
     *)
    val emptyStackMap = "\
          \\t.globl __LLVM_StackMaps\n\
          \__LLVM_StackMaps:\n\
          \\t.byte   3\n\
          \\t.byte   0\n\
          \\t.short  0\n\
          \\t.long   0\n\
          \\t.long   0\n\
          \\t.long   0\n\
          \"

    (* the section for the stack map is OS specific *)
    val stackMapSect = if SMLofNJ.SysInfo.getOSName() = "Linux"
            then "\t.section .llvm_stackmaps,\"a\",@progbits\n"
          else if SMLofNJ.SysInfo.getOSName() = "Darwin"
            then "\t.section __LLVM_STACKMAPS,__llvm_stackmaps\n"
            else raise Fail "system is not linux or macos"

    fun patchAssembly asmFile = if FSys.access(asmFile, [])
          then let
            (* a temp-file name for the assembly code *)
            val tmpFile = Path.file (FSys.tmpName())
            (* copy the input stream to the output stream and insert a
             * global declaration for the stack maps right before the label.
             *)
            fun patch (inS, outS) = let
                  fun lp seenMap = (case TextIO.inputLine inS
                         of SOME ln => let
                              val seenMap = seenMap orelse
                                    if String.isPrefix "__LLVM_StackMaps:" ln
                                      then (
                                        TextIO.output(
                                          outS,
                                          "\t.globl __LLVM_StackMaps\n");
                                        true)
                                      else false
                              in
                                TextIO.output (outS, ln);
                                lp seenMap
                              end
                          | NONE => seenMap
                        (* end case *))
                  in
                    if lp false
                      then ()
                      else ((* no stackmap so add the empty map *)
                        TextIO.output(outS, stackMapSect);
                        TextIO.output(outS, emptyStackMap))
                  end
            in
              (* first we move the assembly file to a temporary name *)
              FSys.rename {old = asmFile, new = tmpFile};
              (* run the patch command *)
              tryFinally (TextIO.openIn tmpFile,
                fn inS => tryFinally (TextIO.openOut asmFile,
                    fn outS => patch (inS, outS),
                    fn outS => TextIO.closeOut outS),
                fn inS => TextIO.closeIn inS);
              (* remove the original file *)
              FSys.remove tmpFile;
              OS.Process.success
            end
          else err ["assembly file '", asmFile, "' is missing\n"]

    fun runtimePath () = let
          val basePath = Path.getParent (Path.dir (CommandLine.name()))
          val rtDir = Path.concat(basePath, "src/runtime")
          val objPath = Path.joinDirFile{dir = rtDir, file = "mml-rt.o"}
          fun error msg = (err msg; raise Error.ERROR)
          in
            if not (FSys.access(rtDir, []))
              then error ["expected runtime library at '", rtDir, "'\n"]
            else if not (FSys.access(objPath, []))
              then error ["runtime library has not been built yet\n"]
              else objPath
          end

  end
