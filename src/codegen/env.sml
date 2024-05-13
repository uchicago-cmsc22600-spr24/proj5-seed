(* env.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Environments for code generation.
 *)

structure Env : sig

    type t

    (* create a new environment for generating code into the specified module *)
    val new : LLVMModule.t -> t

    (* get the LLVM module from the environment *)
    val moduleOf : t -> LLVMModule.t

    (* map a string literal to the global variable that defines it *)
    val stringLit : t * string -> LLVMGlobal.t

    (* `beginBlock (env, blk, params, args)` initializes the environment
     * for processing `blk` by adding the bindings from `params` to `args`.
     *)
    val beginBlock : t * LLVMBlock.t * CFGVar.t list * LLVMVar.t list -> t

    (* return the current block; raises Fail if no block is set *)
    val block : t -> LLVMBlock.t

    (* bind a CFG local variable to a LLVM var *)
    val bind : t * CFGVar.t * LLVMVar.t -> t

    (* lookup a CFG local variable *)
    val lookup : t * CFGVar.t -> LLVMVar.t

    (* lookup a CFG local variable; return NONE if not bound *)
    val find : t * CFGVar.t -> LLVMVar.t option

    (* is a CFG local variable defined in the environment? *)
    val inDomain : t * CFGVar.t -> bool

    (* dump the local variable bindings to stdOut (for debugging) *)
    val dump : string * t -> unit

  end = struct

    structure IR = CFG
    structure VMap = CFGVar.Map
    structure LTy = LLVMType
    structure LV = LLVMVar

    structure StringTbl = HashTableFn (
      struct
        type hash_key = string
        val hashVal = HashString.hashString
        val sameKey : (string * string) -> bool = (op =)
      end)

    datatype t = E of {
        vEnv : LLVMVar.t VMap.map,                      (* variable to LLVM var mapping *)
        module : LLVMModule.t,                          (* the module for the program *)
        strings : LLVMGlobal.t StringTbl.hash_table,    (* table of string literals *)
        blk : LLVMBlock.t option                        (* the current block *)
      }

    fun new m = E{
            vEnv = VMap.empty,
            module = m,
            strings = StringTbl.mkTable (32, Fail "strings"),
            blk = NONE
          }

    fun moduleOf (E{module, ...}) = module

    fun stringLit (E{module, strings, ...}, slit) = (case StringTbl.find strings slit
           of NONE => let
                val id = StringTbl.numItems strings
                val name = "_slit_" ^ Int.toString id
                (* string-object header has its length as a tagged integer in
                 * the first word, and then the data.
                 *)
                val hdr = IntInf.<<(Int.toLarge(size slit), 0w1) + 1
                (* add the string definition to the module *)
                val g = LLVMModule.defineString (module, name, hdr, slit)
                in
                  StringTbl.insert strings (slit, g);
                  g
                end
            | SOME g => g
          (* end case *))

    fun beginBlock (E{module, strings, ...}, blk, params, args) = E{
            vEnv = ListPair.foldl
              (fn (x, v, env) => VMap.insert(env, x, v))
                VMap.empty (params, args),
            module = module,
            strings = strings,
            blk = SOME blk
          }

    fun block (E{blk=SOME blk, ...}) = blk
      | block _ = raise Fail "Env.block: no block set"

    fun bind (E{blk = NONE, ...}, _, _) = raise Fail "Env.bind: no block set"
      | bind (E{vEnv, module, strings, blk}, x, v) = E{
            vEnv = VMap.insert(vEnv, x, v),
            module = module, strings = strings,
            blk = blk
          }

    fun find (E{vEnv, ...}, x) = VMap.find (vEnv, x)

    fun lookup (env, x) = (case find (env, x)
           of SOME v => v
            | NONE => raise Fail(concat["no binding for '", CFGVar.nameOf x, "'"])
          (* end case *))

    fun inDomain (E{vEnv, ...}, x) = VMap.inDomain(vEnv, x)

    (* dump the local variable bindings to stdOut (for debugging) *)
    fun dump (msg, E{vEnv, ...}) = let
          fun prBind (x, x') = print(concat[
                  "  ", CFGVar.nameOf x, " --> ", LV.toString x', "\n"
                ])
          in
            print (concat["***** ", msg, " *****\n"]);
            VMap.appi prBind vEnv;
            print "*****\n"
          end

  end
