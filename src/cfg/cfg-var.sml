(* cfg-var.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *)

structure CFGVar :> sig

    type t = CFG.var

 (* Create a fresh variable *)
    val new : string * PrimType.t -> t

 (* Create a fresh variable by copying from an existing variable. *)
    val copy : t -> t

  (* are two variables the same? *)
    val same : t * t -> bool

  (* the name of the variable *)
    val nameOf : t -> string

  (* a unique string representation of the variable *)
    val toString : t -> string

  (* the variable's type *)
    val typeOf : t -> PrimType.t

  (* properties and flags *)
    val newProp : (t -> 'a) -> {
            clrFn : t -> unit,          (* remove the property from the variable *)
            getFn : t -> 'a,            (* get the property from the variable *)
            peekFn : t -> 'a option,    (* does the variable have the property? *)
            setFn : t * 'a -> unit      (* set the property *)
          }
    val newFlag : unit -> {
            getFn : t -> bool,          (* get the flag value *)
            setFn : t * bool -> unit    (* set the flag value; false removes it *)
          }

  (* sets, maps, anbd tables keyed by variables *)
    structure Set : ORD_SET where type Key.ord_key = t
    structure Map : ORD_MAP where type Key.ord_key = t
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = t

  end = struct

    datatype t = datatype CFGRep.var

    fun new (name, ty) = V{
            name = name,
            id = Stamp.new(),
            ty = ty,
            props = PropList.newHolder()
          }

    fun copy (V{name, ty, ...}) = new (name, ty)

    fun nameOf (V{name, ...}) = name
    fun typeOf (V{ty, ...}) = ty
    fun stamp (V{id, ...}) = id
    fun props (V{props, ...}) = props

    fun compare (v1, v2) = Stamp.compare (stamp v1, stamp v2)
    fun same (v1, v2) = Stamp.same (stamp v1, stamp v2)
    fun hash v = Stamp.hash (stamp v)

    fun toString (V{name, id, ...}) = name ^ Stamp.toString id

    fun newProp initVal = PropList.newProp (props, initVal)

    fun newFlag () = PropList.newFlag (fn (V{props, ...}) => props)

    structure OrdKey =
       struct
          type ord_key = t
          val compare = compare
       end
    structure Map = RedBlackMapFn (OrdKey)
    structure Set = RedBlackSetFn (OrdKey)

    structure HashKey =
        struct
          type hash_key = t
          val hashVal = hash
          val sameKey = same
        end
    structure Tbl = HashTableFn (HashKey)

  end
