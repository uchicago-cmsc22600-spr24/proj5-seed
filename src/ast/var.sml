(* var.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *)

structure Var :> sig

    type t

    (* create a new AST variable with the given name and type *)
    val new : BindTree.ValId.t * Type.ty -> t
    val newTmp : string * Type.ty -> t

    (* create a new primitive operator variable *)
    val prim : BindTree.ValId.t * Type.scheme -> t

    (* create a new variable for a wildcard *)
    val wild : Type.ty -> t

    (* return a variable's name *)
    val nameOf : t -> string

    (* return a unique string representation of a variable; this is formed from the
     * variable's name and its stamp.
     *)
    val toString : t -> string

    (* is a variable a primitive operator? *)
    val isPrim : t -> bool

    (* return a variable's type scheme *)
    val typeOf : t -> Type.scheme

    (* update a variable's type (presumably with a polymorphic scheme) *)
    val updateTy : t * Type.scheme -> unit

    (* are two variables the same? *)
    val same : t * t -> bool

    (* sets, finite maps, and hash tables keyed by variables *)
    structure Set : ORD_SET where type Key.ord_key = t
    structure Map : ORD_MAP where type Key.ord_key = t
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = t

  end = struct

    (* value variables in the AST.
     * Note that the type of a variable is a reference; this is necessary because
     * the type of a recursive polymorphic function is monomorphic in its body,
     * but polymorphic outside.  Thus, we need to be able to update such a
     * function's type after checking its body.
     *)
    datatype t = V of {
        name : string,
        id : Stamp.t,           (* unique stamp that distinguishes this variable *)
        isPrim : bool,          (* true for builtin primitives *)
        ty : Type.scheme ref
      }

    fun newTmp (name, ty) = V{
            name = name,
            id = Stamp.new(),
            isPrim = false,
            ty = ref(Type.TyScm([], ty))
          }

    local
      fun mk isPrim (x, tyScm) = V{
            name = BindTree.ValId.nameOf x,
            id = Stamp.new(),
            isPrim = isPrim,
            ty = ref tyScm
          }
    in
    fun new (id, ty) = mk false (id, Type.TyScm([], ty))
    val prim = mk true
    end

    fun wild ty = V{
            name = "_", id = Stamp.new(),
            isPrim = false,
            ty = ref(Type.TyScm([], ty))
          }

    fun nameOf (V{name, ...}) = name

    fun toString (V{name, id, ...}) = name ^ Stamp.toString id

    fun isPrim (V{isPrim=p, ...}) = p

    fun typeOf (V{ty, ...}) = !ty

    fun updateTy (V{ty, ...}, tyScm) = ty := tyScm

    fun same (V{id=a, ...}, V{id=b, ...}) = Stamp.same(a, b)

    structure Key =
      struct
        type ord_key = t
        fun compare (V{id=a, ...}, V{id=b, ...}) = Stamp.compare(a, b)
      end
    structure Map = RedBlackMapFn (Key)
    structure Set = RedBlackSetFn (Key)

    structure Tbl = HashTableFn (struct
        type hash_key = t
        fun hashVal (V{id, ...}) = Stamp.hash id
        val sameKey = same
      end)

  end
