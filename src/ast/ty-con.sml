(* tycon.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Type constructors.  These include the primitive types, declared types, and
 * the built-in tuple-type constructors.
 *)

structure TyCon : sig

    type t = TypeRep.tycon

    (* return a new primitive tycon *)
    val newPrim : BindTree.tycon * TyVar.t list -> t

    (* return a new user tycon *)
    val new : BindTree.tycon * TyVar.t list -> t

    (* finish the definition of a data-type constructor by sorting its constructors
     * into canonical order.  This function should be called after all of the
     * constructors have been added to the type.  It returns the completed datatype.
     *)
    val finish : t -> TypeRep.usr_tyc

    (* return the name of a tycon *)
    val nameOf : t -> string

    (* return a unique string representation of a tycon *)
    val toString : t -> string

    (* return the arity of the type constructor *)
    val arityOf : t -> int

    (* `isPrim tycon` returns true if `tycon` is a primitive type *)
    val isPrim : t -> bool

    (* `isFun tycon` returns true if `tycon` is the function type constructor *)
    val isFun : t -> bool

    (* `isTuple tycon` returns true if `tycon` is a tuple tycon *)
    val isTuple : t -> bool

    (* `consOf tycon` returns the list of data constructors of the datatype `tycon`.
     * It returns `[]` if `tycon` is an abstract type constructor.
     *)
    val consOf : t -> TypeRep.dcon list

    (* are two type constructors the same? *)
    val same : t * t -> bool

    (* total ordering on type constructors *)
    val compare : t * t -> order

    (* sets, finite maps, and hash tables keyed by type constructors *)
    structure Set : ORD_SET where type Key.ord_key = t
    structure Map : ORD_MAP where type Key.ord_key = t
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = t

  end = struct

    structure Ty = TypeRep

    datatype t = datatype Ty.tycon

    fun newPrim (tycon, tvs) = PrimTyc{
            id = Stamp.new(),
            name = BindTree.TycId.nameOf tycon,
            params = tvs
          }

    fun new (tycon, tvs) = UserTyc{
            id = Stamp.new(),
            name = BindTree.TycId.nameOf tycon,
            params = tvs,
            cons = ref []
          }

    fun finish (UserTyc(dt as {cons, ...})) = let
        (* we order constructors first by constants before functions and,
         * second, by name.
         *)
          fun gt (Ty.DCon{argTys=_::_, ...}, Ty.DCon{argTys=[], ...}) = true
            | gt (Ty.DCon{argTys=[], ...}, Ty.DCon{argTys=_::_, ...}) = false
            | gt (Ty.DCon{name=a, ...}, Ty.DCon{name=b, ...}) = String.>(a, b)
          in
            cons := ListMergeSort.sort gt (!cons);
            dt
          end
      | finish _ = raise Fail "not a datatype"

    fun nameOf (PrimTyc{name, ...}) = name
      | nameOf FunTyc = "->"
      | nameOf (TupleTyc n) = "$" ^ Int.toString n
      | nameOf (UserTyc{name, ...}) = name

    fun toString (PrimTyc{name, id, ...}) = name ^ Stamp.toString id
      | toString FunTyc = "->"
      | toString (TupleTyc n) = "$" ^ Int.toString n
      | toString (UserTyc{name, id, ...}) = name ^ Stamp.toString id

    fun arityOf (PrimTyc{params, ...}) = List.length params
      | arityOf FunTyc = 2
      | arityOf (TupleTyc n) = n
      | arityOf (UserTyc{params, ...}) = List.length params

    fun isPrim (PrimTyc _) = true
      | isPrim _ = false

    fun isFun FunTyc = true
      | isFun _ = false

    fun isTuple (TupleTyc _) = true
      | isTuple _ = false

    fun consOf (UserTyc{cons, ...}) = !cons
      | consOf _ = []

  (* are two type constructors the same? *)
    fun same (PrimTyc{id=a, ...}, PrimTyc{id=b, ...}) = Stamp.same(a, b)
      | same (FunTyc, FunTyc) = true
      | same (TupleTyc n1, TupleTyc n2) = (n1 = n2)
      | same (UserTyc{id=a, ...}, UserTyc{id=b, ...}) = Stamp.same(a, b)
      | same _ = false

    fun compare (PrimTyc{id=a, ...}, PrimTyc{id=b, ...}) = Stamp.compare(a, b)
      | compare (PrimTyc _, _) = LESS
      | compare (_, PrimTyc _) = GREATER
      | compare (FunTyc, FunTyc) = EQUAL
      | compare (FunTyc, _) = LESS
      | compare (_, FunTyc) = GREATER
      | compare (TupleTyc n1, TupleTyc n2) = Int.compare(n1, n2)
      | compare (TupleTyc _, _) = LESS
      | compare (_, TupleTyc _) = GREATER
      | compare (UserTyc{id=a, ...}, UserTyc{id=b, ...}) = Stamp.compare(a, b)

    structure Key =
      struct
        type ord_key = t
        val compare = compare
      end
    structure Map = RedBlackMapFn (Key)
    structure Set = RedBlackSetFn (Key)

    structure Tbl = HashTableFn (struct
        type hash_key = t
        fun hashVal (PrimTyc{id, ...}) = 0w17 + Stamp.hash id
          | hashVal FunTyc = 0w13
          | hashVal (TupleTyc n) = 0w31 + Word.fromInt n
          | hashVal (UserTyc{id, ...}) = 0w53 + Stamp.hash id
        val sameKey = same
      end)

  end
