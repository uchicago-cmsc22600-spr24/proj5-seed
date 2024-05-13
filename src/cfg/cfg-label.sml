(* cfg-label.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *)

structure CFGLabel :> sig

    type t = CFG.label

    datatype label_kind
      = LK_None                         (* placeholder *)
      | LK_Export of string             (* the program's entry function's label *)
      | LK_Entry of CFG.function        (* cluster entry *)
      | LK_Local of CFG.frag            (* local fragment label *)
      | LK_Extern                       (* external (i.e., C) function *)

    (* new labels *)
    val new : string -> t               (* new label with kind LK_None *)
    val newExport : string -> t         (* new exported label with kind LK_None *)
    val newExtern : string -> t         (* new external label (e.g., a runtime label) *)

    (* Create a fresh label by copying from an existing label. *)
    val copy : t -> t

    (* are two variables the same? *)
    val same : t * t -> bool

    (* the name of the variable *)
    val nameOf : t -> string

    (* a unique string representation of the label *)
    val toString : t -> string

    (* the label's kind *)
    val kindOf : t -> label_kind

    (* set the label's kind *)
    val setKind : t * label_kind -> unit

    (* tests for various kinds of labels *)
    val isExtern : t -> bool
    val isEntry : t -> bool
    val isExport : t -> bool

    (* properties and flags *)
    val newProp : (t -> 'a) -> {
            clrFn : t -> unit,          (* remove the property from the label *)
            getFn : t -> 'a,            (* get the property from the label *)
            peekFn : t -> 'a option,    (* does the label have the property? *)
            setFn : t * 'a -> unit      (* set the property *)
          }
    val newFlag : unit -> {
            getFn : t -> bool,          (* get the flag value *)
            setFn : t * bool -> unit    (* set the flag value; false removes it *)
          }

    (* sets, maps, anbd tables keyed by labels *)
    structure Set : ORD_SET where type Key.ord_key = t
    structure Map : ORD_MAP where type Key.ord_key = t
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = t

  end = struct

    datatype t = datatype CFGRep.label

    datatype label_kind = datatype CFGRep.label_kind

    fun mk (name, export, kind) = L{
            name = name,
            id = Stamp.new(),
            kind = ref kind,
            export = export,
            props = PropList.newHolder()
          }

    fun new name = mk (name, false, LK_None)
    fun newExport name = mk (name, true, LK_Export name)
    fun newExtern name = mk (name, false, LK_Extern)

    fun copy (L{name, export, kind, ...}) = mk (name, export, !kind)

    fun nameOf (L{name, ...}) = name
    fun stamp (L{id, ...}) = id
    fun props (L{props, ...}) = props

    fun compare (l1, l2) = Stamp.compare (stamp l1, stamp l2)
    fun same (l1, l2) = Stamp.same (stamp l1, stamp l2)
    fun hash l = Stamp.hash (stamp l)

    fun toString (L{name, export=true, ...}) = "$" ^ name
      | toString (L{name, id, ...}) = concat["$", name, Stamp.toString id]

    fun kindOf (L{kind, ...}) = !kind
    fun setKind (L{kind, ...}, k) = kind := k

    fun isExtern lab = (case kindOf lab of LK_Extern => true | _ => false)
    fun isEntry lab = (case kindOf lab of LK_Entry _ => true | _ => false)
    fun isExport (L{export, ...}) = export

    fun newProp initVal = PropList.newProp (props, initVal)

    fun newFlag () = PropList.newFlag props

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
