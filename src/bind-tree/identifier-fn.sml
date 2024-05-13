(* identifier-fn.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Functor for generating unique classes of identifiers.
 *)

functor IdentifierFn () : sig

    type t

    (* define a new variable with the given name *)
    val new : Atom.atom -> t

    (* the source-code name of the identifier *)
    val nameOf : t -> string
    (* a unique string representation of the identifier *)
    val toString : t -> string

    (* properties (see http://smlnj.org/doc/smlnj-lib/Util/str-PropList.html) *)
    val newFlag : unit -> {
            getFn : t -> bool,
            setFn : t * bool -> unit
          }
    val newProp : (t -> 'prop) -> {
            peekFn : t -> 'prop option,
            getFn  : t -> 'prop,
            setFn  : t * 'prop -> unit,
            clrFn  : t -> unit
          }

    (* comparisons *)
    val compare : t * t -> order
    val same : t * t -> bool

    (* finite sets of identifiers *)
    structure Set : ORD_SET where type Key.ord_key = t
    (* finite maps with identifier keys *)
    structure Map : ORD_MAP where type Key.ord_key = t
    (* hash tables with identifier keys *)
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = t

  end = struct

    datatype t = ID of {
        name : string,          (* name of identifier *)
        stamp : Stamp.t,        (* unique stamp used to distinguish binding occurrences *)
        props : PropList.holder (* holder for properties *)
      }

    fun new name = ID{
            name = Atom.toString name,
            stamp = Stamp.new(),
            props = PropList.newHolder()
          }

    fun nameOf (ID{name, ...}) = name

    fun toString (ID{name, stamp, ...}) = name ^ Stamp.toString stamp

    fun newFlag () = PropList.newFlag (fn (ID{props, ...}) => props)

    fun newProp initialize = PropList.newProp (
          fn (ID{props, ...}) => props,
          initialize)

    fun compare (ID{stamp = id1, ...}, ID{stamp = id2, ...}) = Stamp.compare(id1, id2)

    fun same (ID{stamp = id1, ...}, ID{stamp = id2, ...}) = Stamp.same(id1, id2)

    structure Set = RedBlackSetFn (
      struct
        type ord_key = t
        val compare = compare
      end)
    structure Map = RedBlackMapFn (
      struct
        type ord_key = t
        val compare = compare
      end)
    structure Tbl = HashTableFn (
      struct
        type hash_key = t
        fun hashVal (ID{stamp, ...}) = Stamp.hash stamp
        val sameKey = same
      end)

  end
