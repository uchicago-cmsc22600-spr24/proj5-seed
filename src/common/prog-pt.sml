(* prog-pt.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Program point labels.  These are represented as property list holders.
 * We assign unique IDs (stored as a property) on demand.
 *)

structure ProgPt :> sig

  (* a unique point in a program *)
    type t

  (* create a new program point *)
    val new : unit -> t

  (* comparisons and hashing *)
    val same : (t * t) -> bool
    val compare : (t * t) -> order
    val hash : t -> word

  (* return a string representation of a program point *)
    val toString : t -> string

  (* return a string representation of a program point only if it is semantically
   * significant (i.e., if it has some properties associated with it)
   *)
    val label : t -> string option

  (* create a new property to associate with program points *)
    val newProp : (t -> 'a) -> {
            clrFn  : t -> unit,
            getFn  : t -> 'a,
            setFn : (t * 'a) -> unit,
            peekFn : t -> 'a option
          }

  (* create a new flag to associate with program points *)
    val newFlag : unit -> {
            getFn : t -> bool,
            setFn : (t * bool) -> unit
          }

  (* sets, maps, and hash tables that are keyed by program points *)
    structure Set : ORD_SET where type Key.ord_key = t
    structure Map : ORD_MAP where type Key.ord_key = t
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = t

  end = struct

    type t = PropList.holder

    val count = ref 0w0

  (* all program points have an implicit unique ID property, which is used as a key *)
    fun newId () = let val n = !count in count := n+0w1; n end

    val {getFn=getId, ...} = PropList.newProp (fn h => h, fn _ => newId())

    fun new () = PropList.newHolder()

    fun compare (a, b) = Word.compare(getId a, getId b)
    val hash = getId
    val same = PropList.sameHolder

    fun toString (pt : t) = Format.format "P%04x" [Format.WORD(getId pt)]
    fun label pt = if PropList.hasProps pt then SOME(toString pt) else NONE

    fun newProp f = PropList.newProp (fn h => h, f)
    fun newFlag () = PropList.newFlag (fn h => h)

    structure K = struct
        type ord_key = t
        val compare = compare
      end

    structure Set = RedBlackSetFn (K)
    structure Map = RedBlackMapFn (K)
    structure Tbl = HashTableFn (
      struct
        type hash_key = t
        val hashVal = hash
        val sameKey = same
      end)

  end
