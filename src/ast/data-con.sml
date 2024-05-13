(* data-con.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Operations on data constructors
 *)

structure DataCon : sig

    type t = Type.dcon

    (* create a new data constructor and add it to the list of data constructors
     * of its type constructor.
     *)
    val new : Type.tycon * BindTree.conid * Type.ty list -> t

    (* return the name of a data constructor *)
    val nameOf : t -> string

    (* return a unique string representation of a variable *)
    val toString : t -> string

    (* return true if this is a nullary data constructor *)
    val isNullary : t -> bool

    (* return the arity (number of arguments) of the data constructor *)
    val arityOf : t -> int

    (* return the type constructor that the data constructor belongs to *)
    val tycOf : t -> Type.tycon

    (* return the type signature of a data constructor.  The result is a triple
     * `(tvs, tys, ty)`, where the `tvs` are the type parameters (bound in the
     * owning type constructor, the `tys` are the type arguments (`nil` for
     * nullary constructors), and `ty` is the type of a value constructed by
     * the data constructor.  This triple is essentially a deconstruction of the
     * type scheme returned by `typeOf`.
     *)
    val tySigOf : t -> Type.tyvar list * Type.ty list * Type.ty

    (* return the polymorphic type of the data constructor.  This will be a
     * curried function type if the data constructor is not nullary.
     *)
    val typeOf : t -> Type.scheme

    (* are two data constructors the same? *)
    val same : t * t -> bool

    (* sets, finite maps, and hash tables keyed by data constructors *)
    structure Set : ORD_SET where type Key.ord_key = t
    structure Map : ORD_MAP where type Key.ord_key = t
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = t

  end = struct

    structure Ty = Type

    datatype t = datatype Ty.dcon

    fun new (Ty.UserTyc dt, id, optTy) = let
          val {cons, ...} = dt
          val dcon = DCon{
                  id = Stamp.new(),
                  name = BindTree.ConId.nameOf id,
                  argTys = optTy,
                  dty = dt
                }
          in
            cons := dcon :: !cons;
            dcon
          end
      | new _ = raise Fail "compiler error: attempt to add dcon to non-user type"

    fun nameOf (DCon{name, ...}) = name

    fun toString (DCon{name, id, ...}) = name ^ Stamp.toString id

    fun arityOf (DCon{argTys, ...}) = List.length argTys

    fun isNullary (DCon{argTys=[], ...}) = true
      | isNullary _ = false

    fun tycOf (DCon{dty, ...}) = Type.UserTyc dty

    fun tySigOf (DCon{argTys, dty, ...}) = let
          val tyParams = #params dty
          val resTy = Ty.ConTy(Ty.UserTyc dty, List.map Ty.VarTy tyParams)
          in
            (tyParams, argTys, resTy)
          end

    fun typeOf dc = let
          val (tyParams, tyArgs, resTy) = tySigOf dc
          in
            Ty.TyScm(tyParams, List.foldr Ty.funTy resTy tyArgs)
          end

  (* are two data constructors the same? *)
    fun same (DCon{id=a, ...}, DCon{id=b, ...}) = Stamp.same(a, b)

    structure Key =
      struct
        type ord_key = t
        fun compare (DCon{id=a, ...}, DCon{id=b, ...}) = Stamp.compare(a, b)
      end
    structure Map = RedBlackMapFn (Key)
    structure Set = RedBlackSetFn (Key)

    structure Tbl = HashTableFn (struct
        type hash_key = t
        fun hashVal (DCon{id, ...}) = Stamp.hash id
        val sameKey = same
      end)

  end
