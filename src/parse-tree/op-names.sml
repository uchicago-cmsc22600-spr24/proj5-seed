(* op-names.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Operator names
 *)

structure OpNames =
  struct

    val asgnId = Atom.atom ":="
    val eqlId = Atom.atom "=="
    val neqId = Atom.atom "!="
    val ltId = Atom.atom "<"
    val lteId = Atom.atom "<="
    val plusId = Atom.atom "+"
    val minusId = Atom.atom "-"
    val timesId = Atom.atom "*"
    val divId = Atom.atom "/"
    val modId = Atom.atom "%"

    val derefId = Atom.atom "!"
    val negId = Atom.atom "unary -"     (* !! different from subId !! *)

  end
