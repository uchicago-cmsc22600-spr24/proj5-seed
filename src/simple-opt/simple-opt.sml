(* simple-opt.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *)

structure SimpleOpt : sig

    (* `transform say prog` performs the optimization passes on the program.
     * The function `say` is used to report progress.
     *)
     val transform : (string list -> unit) -> SimpleAST.program -> SimpleAST.program

  end = struct

  (* list of passes *)
    val passes = [
            ("let-float", LetFloat.transform),
            ("contract", Contract.transform)
          ]

    fun transform say prog = let
          fun apply ((name, transform), prog) = (
                say [name, "\n"];
                transform prog)
          in
            List.foldl apply prog passes
          end

  end
