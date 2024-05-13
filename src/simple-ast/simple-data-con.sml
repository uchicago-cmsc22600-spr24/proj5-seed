(* simple-data-con.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Simple AST data constructors.
 *)

structure SimpleDataCon : sig

    type t

    (* representation of data constructors *)
    datatype con_rep
      = Enum of int                     (* tagged integer (for nullary constructors) *)
      | Transparent of PrimType.kind    (* single-argument constructor that uses
                                         * its argument representation is its
                                         * representation.
                                         *)
      | Tuple of int                    (* representation as a tuple; the argument
                                         * is the arity
                                         *)
      | TaggedTuple of {                (* representation as a tagged tuple *)
            tag : int,
            arity : int
          }

    (* define a new data-constructor *)
    val new : string * PrimType.t list * con_rep -> t

    (* return the constructor's name *)
    val nameOf : t -> string

    (* return a unique string representation of the constructor *)
    val toString : t -> string

    (* compare constructors for equality *)
    val same : t * t -> bool

    (* return true if the constructor is nullary (which means that its rep will be
     * `Enum n` for some `n`.
     *)
    val isNullary : t -> bool

    (* return true if the constructor is a constructor function with `Transparent`
     * representation.  In this case, its representation (boxed vs. unboxed) is
     * determined by its argument type.
     *)
    val isTransparent : t -> bool

    (* return the representation of the constructor *)
    val repOf : t -> con_rep

    (* return the kind of the constructor's representation type *)
    val kindOf : t -> PrimType.kind

    (* sets, finite maps, and hash tables keyed by variables *)
    structure Set : ORD_SET where type Key.ord_key = t
    structure Map : ORD_MAP where type Key.ord_key = t
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = t

  end = struct

    datatype t = DC of {
        name : string,
        id : Stamp.t,
        argTys : PrimType.t list,
        rep : con_rep
      }

    and con_rep
      = Enum of int
      | Transparent of PrimType.kind
      | Tuple of int
      | TaggedTuple of {tag : int, arity : int}

    fun new (name, argTys, rep) = DC{
            name = name,
            id = Stamp.new(),
            argTys = argTys,
            rep = rep
          }

    fun nameOf (DC{name, ...}) = name

    fun toString (DC{name, id, ...}) = name ^ Stamp.toString id

    fun same (DC{id=a, ...}, DC{id=b, ...}) = Stamp.same(a, b)

    fun isNullary (DC{rep=Enum _, ...}) = true
      | isNullary _ = false

    fun isTransparent (DC{rep=Transparent _, ...}) = true
      | isTransparent _ = false

    fun repOf (DC{rep, ...}) = rep

    fun kindOf dc = (case repOf dc
           of Enum _ => PrimType.Unboxed
            | Transparent k => k
            | _ => PrimType.Boxed
          (* end case *))

    structure Key =
      struct
        type ord_key = t
        fun compare (DC{id=a, ...}, DC{id=b, ...}) = Stamp.compare(a, b)
      end
    structure Map = RedBlackMapFn (Key)
    structure Set = RedBlackSetFn (Key)

    structure Tbl = HashTableFn (struct
        type hash_key = t
        fun hashVal (DC{id, ...}) = Stamp.hash id
        val sameKey = same
      end)

  end
