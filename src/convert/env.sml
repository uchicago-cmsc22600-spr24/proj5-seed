(* env.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * The translation environment for closure conversion.  The environment has both
 * imperative and functional components.
 *)

structure Env : sig

    type t

    (* create a new environment *)
    val new : unit -> t

    (** Work list operations **)

    (* a SimpleAST function definition *)
    type lambda = PrimType.t * SimpleVar.t * SimpleVar.t list * SimpleAST.exp

    (* add a SimpleAST function to the work list *)
    val addFunToWL : t * lambda -> unit

    (* get the next function from the work list; returns `NONE` if the list is
     * empty.
     *)
    val getFunFromWL : t -> lambda option

    (** CFG function operations **)

    (* `newFun (env, f, isSelfTailRec)` prepares the state of `env` to process the
     * first-order SimpleAST function `f`.  The environment will have a definition
     * for the header label if the function is self-tail recursive, an empty list
     * of fragments, and an empty variable mapping.
     *)
    val newFun : t * SimpleVar.t * bool -> t

    (* add a CFG function to the list of functions *)
    val addFun : t * CFGFunct.t -> unit

    (* get the list of functions from the environment; the functions are returned in the
     * reverse order in which they were added.  Note that this operation clears the list.
     *)
    val getFuns : t -> CFGFunct.t list

    (* get the current function *)
    val currentFun : t -> SimpleVar.t

    (* get the header-fragment label; this raises `Fail` if no such label is set *)
    val getHeaderLable : t -> CFGLabel.t

    (* record a non-entry fragment in the current function *)
    val addFrag : t * CFG.frag -> unit

    (* get the fragments for the current function; the fragments are returned in the
     * reverse order in which they were added.  Note that this operation clears the list.
     *)
    val getFrags : t -> CFG.frag list

    (* reset the environment to process a new fragment; the resulting environment has
     * an empty variable mapping.
     *)
    val newFrag : t -> t

    (** variable operations **)

    (* lookup the CFG value that a SimpleAST variable maps to in the environment *)
    val lookup : t * SimpleVar.t -> CFG.value

    (* bind a variable to a CFG value *)
    val bind : t * SimpleVar.t * CFG.value -> t

  end = struct

    structure SV = SimpleVar
    structure VMap = SimpleVar.Map
    structure CV = CFGVar

    type lambda = PrimType.t * SimpleVar.t * SimpleVar.t list * SimpleAST.exp

    (* the mutable parts of the environment *)
    datatype st = St of {
        wl : lambda list ref,           (* the work list *)
        funs : CFGFunct.t list ref,     (* the translated functions *)
        frags : CFGFrag.t list ref      (* the fragments for the current function *)
      }

    datatype t = Env of {
        st : st,                        (* the mutable part of the environment *)
        curFn : SV.t,                   (* the current function being translated *)
        hdrLab : CFGLabel.t option,     (* the optional header label for the current
                                         * function
                                         *)
        vMap : CFG.value VMap.map       (* mapping from SimpleAST variables to CFG *)
      }

    fun new () = Env{
            st = St{wl = ref [], funs = ref [], frags = ref []},
            curFn = SV.new("*dummy*", PrimType.anyTy),
            hdrLab = NONE,
            vMap = VMap.empty
          }

    fun addFunToWL (Env{st=St{wl, ...}, ...}, lam) =
          wl := lam :: !wl

    fun getFunFromWL (Env{st=St{wl, ...}, ...}) = (case !wl
           of [] => NONE
            | lam::lams => (wl := lams; SOME lam)
          (* end case *))

    fun addFun (Env{st=St{funs, ...}, ...}, func) = (funs := func :: !funs)

    fun getFuns (Env{st=St{funs, ...}, ...}) = !funs before (funs := [])

    fun newFun (Env{st as St{frags, ...}, ...}, f, isSelfTailRec) = (
          frags := [];
          Env{st = st, curFn = f,
              hdrLab = if isSelfTailRec
                then SOME(CFGLabel.new(SV.toString f ^ "_hdr"))
                else NONE,
              vMap = VMap.empty
            })

    fun currentFun (Env{curFn, ...}) = curFn

    fun getHeaderLable (Env{hdrLab=SOME lab, ...}) = lab
      | getHeaderLable (Env{curFn, ...}) = raise Fail(concat[
            "no header label for '", SV.toString curFn, "'"
          ])

    fun addFrag (Env{st=St{frags, ...}, ...}, frag) = (frags := frag :: !frags)

    fun getFrags (Env{st=St{frags, ...}, ...}) = !frags before (frags := [])

    fun newFrag (Env{st, curFn, hdrLab, ...}) = Env{
            st = st,
            curFn = curFn,
            hdrLab = hdrLab,
            vMap = VMap.empty
          }

    fun lookup (Env{vMap, ...}, x) = (case VMap.find(vMap, x)
           of SOME v => v
            | NONE => raise Fail(concat["lookup: no mapping for '", SV.toString x, "'"])
          (* end case *))

    fun bind (Env{st, curFn, hdrLab, vMap}, x, v) = Env{
            st = st,
            curFn = curFn,
            hdrLab = hdrLab,
            vMap = VMap.insert (vMap, x, v)
          }

  end
