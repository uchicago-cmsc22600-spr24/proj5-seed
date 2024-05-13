(* llvm-rep.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Internal representations of LLVM objects.
 *)

structure LLVMRep =
  struct

    (* simplified LLVM types *)
    datatype ty
      = Void
      | Int1                            (* a 1 bit integer. for bools *)
      | Int8                            (* an 8 bit integer *)
      | Int32                           (* a 32 bit integer *)
      | Int64                           (* a 64 bit integer *)
      | Ptr of ty                       (* pointer; internally, we use typed pointers
                                         * to make it easier to generate some operations,
                                         * but these map to the LLVM opaque pointer
                                         * type in address space 0.
                                         *)
      | GCPtr of ty                     (* garbage collected pointer (address space 1) *)
      | Array of int * ty               (* int = num elms *)
      | Struct of ty list
      | Token                           (* unique returned by GC wrapped function calls *)
      | VarArg                          (* varargs; used for the GC intrinsics *)
      | Proto of ty * ty list           (* LLVM function type; for globals only *)

    datatype global = Glob of {         (* global names (e.g., function names) *)
          name : Atom.atom,             (* variable name (including "@") *)
          ty : ty
        }

    datatype label = Lab of {
          name : string,                (* name of label *)
          id : Stamp.t                  (* unique stamp *)
        }

    datatype reg = Reg of {
          name : string,                (* register name; defaults to "" *)
          id : int,                     (* unique ID; generated in LLVReg *)
          ty : ty                       (* type *)
        }

    datatype var
      = VReg of reg                     (* LLVM local variable *)
      | IConst of ty * IntInf.int       (* integer literal *)
      | Global of global
      | Label of label
      | Comment of string

    datatype module = Module of {
          globals : global_def list ref, (* global definitions (other than functions) *)
          funs : func list ref          (* the functions in the module *)
        }

    and global_def
      = ExternFn of global
      | ExternVar of global
      | Data of global * var list

    and func = Func of {
          name : global,                (* the function's global name *)
          exported : bool,              (* true if the function is called by C *)
          params : reg list,            (* the LLVM pseudo registers for its parameters *)
          entry : block option ref,     (* entry block of function; the option is
                                         * necessary to tie the recursive knot
                                         *)
          body : block list ref         (* list of non-entry blocks *)
        }

    and block = Blk of {
          owner : func,                 (* the enclosing function *)
          name : label,                 (* the block's label *)
          phis : phi list ref,          (* phi nodes for block *)
          body : instr list ref,        (* instructions in the block; while the block
                                         * is open, these are in reverse order.
                                         *)
          closed : bool ref             (* has the block been closed off by a terminator? *)
        }

    and phi = Phi of reg * (var * label) list   (* phi instruction *)

    and instr = Instr of {
          result : reg option,
          rator : operation,
          args : var list
        }

    and operation
      = CastOp                          (* cast to a different type *)
      | CallOp of var list              (* returns a token value.
                                         * vars = pointers live during the call
                                         *)
      | TailCallOp                      (* normal LLVM call with "tail" annotation *)
      | RetValOp                        (* retrieves the retval of a token *)
      | RelocOp                         (* retrieves located livein from a token *)
      (* integer arithmetic. both args must be the same int type *)
      | AddOp                           (* signed addition *)
      | SubOp                           (* signed subtraction *)
      | MulOp                           (* signed multiplication *)
      | DivOp                           (* signed division *)
      | RemOp                           (* signed remainder *)
      | AShftROp                        (* arithmetic shift right *)
      | ShftLOp                         (* shift left *)
      | OrOp                            (* bitwise inclusive or *)
      | XorOp                           (* bitwise exclusive or *)
      | AndOp                           (* bitwise and *)
      (* comparisons. both args must be of same type. *)
      | EquOp                           (* integer or pointer == *)
      | NEqOp                           (* integer or pointer != *)
      | GteOp                           (* signed integer >= *)
      | GtOp                            (* signed integer > *)
      | LtOp                            (* signed integer < *)
      | LteOp                           (* signed integer <= *)
      | ULtOp                           (* unsigned integer < *)
      (* memory/address operations *)
      | LoadOp                          (* read from memory. [pointer] *)
      | StoreOp                         (* write to memory. [pointer, value], no result *)
      | GetElemPtrOp                    (* getelementptr *)
      (* control-flow at end of block *)
      | Return                          (* return. [] or [var] *)
      | Goto                            (* unconditional branch. [label] *)
      | CondBr                          (* conditional branch. [cond : i1, trueLabel, falseLabel] *)
      (* debugging *)
      | CommentOp                       (* prints a comment. [comment] *)

  (* Make sure that identifiers are correct LLVM identifiers *)
    local
      fun isIdChr #"." = true
        | isIdChr #"$" = true
        | isIdChr #"_" = true
        | isIdChr #"-" = true
        | isIdChr c = Char.isAlphaNum c
    in
    fun mangle s = let
          fun cvt (c, cs) = if isIdChr c
                then c :: cs
                else #"$" :: cs
          in
            String.implode (CharVector.foldr cvt [] s)
          end
    end (* local *)

  end
