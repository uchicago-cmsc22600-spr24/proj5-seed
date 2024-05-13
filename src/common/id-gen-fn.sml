(* id-gen-fn.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * A functor to support canonical IDs for identifiers when dumping IRs.
 *)

functor IdGenFn (Tbl : MONO_HASH_TABLE) : sig
    type item = Tbl.Key.hash_key
    type id = SExp.value
    (* create a new item to ID mapping *)
    val new : unit -> item -> id
  end = struct

    type item = Tbl.Key.hash_key
    type id = SExp.value

    fun new () = let
          val nextId = ref 0
          val idTbl = Tbl.mkTable (32, Fail "IdGen")
          val find = Tbl.find idTbl
          val ins = Tbl.insert idTbl
          in
            fn item => (case find item
                 of NONE => let
                      val id = !nextId
                      val sexp = SExp.INT(IntInf.fromInt id)
                      in
                        nextId := id + 1;
                        ins (item, sexp);
                        sexp
                      end
                  | SOME id => id
                (* end case *))
          end

  end (* functor IdGenFn *)
