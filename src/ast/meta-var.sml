(* meta-var.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *)

structure MetaVar : sig

    type t

    (* create a fresh meta variable at the given lambda depth *)
    val fresh : int -> t

    (* return a unique string representation of a meta variable *)
    val toString : t -> string

    (* return true if the first meta-variable is bound at a deeper lambda depth *)
    val isDeeper : t * t -> bool

    (* instantiate a meta variable; raises Fail if already instantiated *)
    val instantiate : t * TypeRep.ty -> unit

    (* are two meta variables the same? *)
    val same : t * t -> bool

    (* finite maps and hash tables keyed by meta variables *)
    structure Map : ORD_MAP where type Key.ord_key = t
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = t

  end = struct

    structure Ty = TypeRep

    datatype t = datatype Ty.meta_var

    fun fresh d = MV{info = ref(Ty.UNIV d), id = Stamp.new()}

    fun toString (MV{id, info}) = (case !info
           of Ty.UNIV d => concat["$", Stamp.toString id, "@", Int.toString d]
            | Ty.INST _ => "$" ^ Stamp.toString id
          (* end case *))

    fun isDeeper (MV{info=ref(Ty.UNIV d1), ...}, MV{info=ref(Ty.UNIV d2), ...}) =
	  d1 > d2
      | isDeeper _ = raise Fail "isDeeper: instantiated meta variable"

    fun instantiate (MV{info, ...}, ty) = (case !info
	   of Ty.UNIV _ => info := Ty.INST(Ty.prune ty)
	    | _ => raise Fail "instantiate"
	  (* end case *))

    fun same (MV{id=a, ...}, MV{id=b, ...}) = Stamp.same(a, b)

    structure Map = RedBlackMapFn (
      struct
	type ord_key = t
	fun compare (MV{id = a, ...}, MV{id = b, ...}) = Stamp.compare(a, b)
      end)

    structure Tbl = HashTableFn (struct
        type hash_key = t
        fun hashVal (MV{id, ...}) = Stamp.hash id
        val sameKey = same
      end)

  end
