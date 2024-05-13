(* cfg-frag.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Utility functions for CFG fragments.
 *)

structure CFGFrag : sig

    type t

    (* build a fragment from a label, parameter list, and body *)
    val new : CFGLabel.t * CFGVar.t list * CFG.exp -> t

    (* project out the components of a fragment *)
    val labelOf : t -> CFGLabel.t
    val paramsOf : t -> CFGVar.t list
    val bodyOf : t -> CFG.exp

    (* the name of the fragment's label *)
    val nameOf : t -> string

    (* properties and flags; note that these are actually attached to the
     * label of the fragment, but these functions allow more convenient
     * access.
     *)
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

  end = struct

    datatype t = datatype CFGRep.frag

    fun new (lab, params, body) = let
          val frag = FRAG{lab=lab, params=params, body=body}
          in
            CFGLabel.setKind(lab, CFGLabel.LK_Local frag);
            frag
          end

    fun labelOf (FRAG{lab, ...}) = lab
    fun paramsOf (FRAG{params, ...}) = params
    fun bodyOf (FRAG{body, ...}) = body

    fun nameOf frag = CFGLabel.nameOf(labelOf frag)

    local
      fun propsOf (FRAG{lab=CFGRep.L{props, ...}, ...}) = props
    in
    fun newProp initVal = PropList.newProp (propsOf, initVal)
    fun newFlag () = PropList.newFlag propsOf
    end (* local *)

  end
