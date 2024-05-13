(* tyvar.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * User defined type variables
 *)

structure TyVar :> sig

    type t

  (* create a new type variable with the given name *)
    val new : BindTree.tyvar -> t

  (* create a new type variable that has the same name as its argument *)
    val copy : t -> t

  (* create a fresh type variable *)
    val fresh : unit -> t

  (* return the name of a type variable *)
    val nameOf : t -> string

  (* return a unique string representation of a type variable *)
    val toString : t -> string

  (* are two type variables the same? *)
    val same : t * t -> bool

  (* sets, finite maps, and hash tables keyed by type variables *)
    structure Set : ORD_SET where type Key.ord_key = t
    structure Map : ORD_MAP where type Key.ord_key = t
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = t

  end = struct

    datatype t = TV of {
        id : Stamp.t,           (* unique stamp for the variable *)
        name : string           (* name of source-language type variable that this represents *)
      }

    fun new tv = TV{id = Stamp.new(), name = BindTree.TyVar.nameOf tv}

    fun copy (TV{name, ...}) = TV{id = Stamp.new(), name = name}

    local
      val count = ref 0
    in
    fun fresh () = let
          val n = !count
          in
            count := n + 1;
            TV{name = "t" ^ Int.toString n, id = Stamp.new()}
          end
    end (* local *)

    fun nameOf (TV{name, ...}) = name

    fun toString (TV{id, name}) = name ^ Stamp.toString id

    fun same (TV{id=a, ...}, TV{id=b, ...}) = Stamp.same(a, b)

    local
      structure Ord = struct
          type ord_key = t
          fun compare (TV{id=a, ...}, TV{id=b, ...}) = Stamp.compare(a, b)
        end
    in
    structure Set = RedBlackSetFn (Ord)
    structure Map = RedBlackMapFn (Ord)
    end

    structure Tbl = HashTableFn (struct
        type hash_key = t
        fun hashVal (TV{id, ...}) = Stamp.hash id
        val sameKey = same
      end)

  end
