(* simple-var.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Simple AST variables.
 *)

structure SimpleVar : sig

    (* abstract type of SimpleAST variables *)
    type t

    (* a pre-defined variable that serves as the name of the program.  This variable
     * is useful when we want to treat the program as if it were a function.
     *)
    val progName : t

    (* define a new variable with the given name and type *)
    val new : string * PrimType.t -> t

    (* `fresh prefix` returns a function for generating fresh variables that have
     * names of the form `prefixN`, where `N` is a sequence number.
     *)
    val fresh : string -> PrimType.t -> t

    (* copy a variable; this operation copies the variable's name and type,
     * but otherwise produces a new variable.
     *)
    val copy : t -> t

    (* return the variable's name *)
    val nameOf : t -> string

    (* return a unique string representation for the variable *)
    val toString : t -> string

    (* return the variable's type *)
    val typeOf : t -> PrimType.t

    (* Census support *)
    val clearUse : t -> unit            (* set use count to zero *)
    val addUse : t * int -> unit        (* add to use count *)
    val incUse : t -> unit              (* increment count by one *)
    val decUse : t -> unit              (* decrement count by one *)
    val useCntOf : t -> int             (* return the variable's use count *)
    val unused : t -> bool              (* returns true if variable is unused *)

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

    (* are two variables the same? *)
    val same : t * t -> bool

    (* sets, maps, and tables keyed by variables *)
    structure Set : ORD_SET where type Key.ord_key = t
    structure Map : ORD_MAP where type Key.ord_key = t
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = t

  end = struct

    datatype t = V of {
        name : string,                  (* name *)
        id : Stamp.t,                   (* unique ID *)
        ty : PrimType.t,                (* the variable's type *)
        useCnt : int ref,               (* count of use occurrences; set by census *)
        props : PropList.holder         (* hook for additional properties *)
      }

    fun new (name, ty) = V{
            name = name,
            id = Stamp.new(),
            ty = ty,
            useCnt = ref 0,
            props = PropList.newHolder()
          }

    val progName = new ("_mml_entry", PrimType.ptrTy)

    fun fresh prefix = let
          val cnt = ref 0
          in
            fn ty => let
                val n = !cnt
                in
                  cnt := n+1;
                  new (prefix ^ Int.toString n, ty)
                end
          end

    fun copy (V{name, ty, ...}) = new (name, ty)

    fun nameOf (V{name, ...}) = name

    fun toString (V{name, id, ...}) = name ^ Stamp.toString id

    fun typeOf (V{ty, ...}) = ty

    fun clearUse (V{useCnt, ...}) = useCnt := 0
    fun addUse (V{useCnt, ...}, n) = useCnt := !useCnt + n
    fun incUse x = addUse (x, 1)
    fun decUse x = addUse (x, ~1)
    fun useCntOf (V{useCnt, ...}) = !useCnt
    fun unused x = (useCntOf x = 0)

    local
      fun stamp (V{id, ...}) = id
    in
    fun compare (x, y) = Stamp.compare (stamp x, stamp y)
    fun same (x, y) = Stamp.same (stamp x, stamp y)
    fun hash x = Stamp.hash (stamp x)
    end (* local *)

    local
      fun propsOf (V{props, ...}) = props
    in
    fun newProp initVal = PropList.newProp (propsOf, initVal)
    fun newFlag () = PropList.newFlag propsOf
    end (* local *)

  (* sets, maps, and tables keyed by variables *)
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
