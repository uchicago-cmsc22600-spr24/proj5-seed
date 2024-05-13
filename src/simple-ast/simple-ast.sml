(* simple-ast.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * A simplified monadic-normal-form representation of MiniML programs.
 *
 * The SimpleAST IR has the following properties:
 *
 *   - all intermediate results have names
 *   - all arguments are values
 *   - curried function definitions have been replaced by nested definitions
 *   - tuple patterns have been converted to selection operations
 *   - built-in operators have been converted to primitive operators
 *   - data constructors are applied to a full complement of arguments
 *)

structure SimpleAST =
  struct

    type var = SimpleVar.t
    type dcon = SimpleDataCon.t
    datatype prim = datatype Prim.t             (* primitive operators *)
    datatype cond = datatype PrimCond.t         (* conditional tests *)

    datatype value
      = V_VAR of var                            (* variables *)
      | V_CON of dcon                           (* nullary constructors *)
      | V_INT of IntInf.int                     (* integer literals *)
      | V_STR of string                         (* string literals *)

    datatype exp
      = E of ProgPt.t * exp_rep                 (* labeled expression *)

    and exp_rep
      = E_LET of var * rhs * exp                (* `let x = rhs in e` *)
      | E_FUN of PrimType.t * var * var list * exp * exp
                                                (* `fun f xs = e in e'`; the type
                                                 * argument is the function's return
                                                 * type.
                                                 *)
      | E_APPLY of var * value list             (* `f (vs)`; note that `f` is a variable *)
      | E_IF of cond * value list * exp * exp   (* `if cond(vs) then e1 else e2` *)
      | E_CASE of value * (pat * exp) list      (* `case` v `of` rules *)
      | E_RET of value                          (* `ret vs` *)

    and rhs
      = R_EXP of exp                            (* right-hand-side expression *)
      | R_PRIM of prim * value list             (* primitive operator application *)
      | R_CALL of Runtime.t * value list        (* call to runtime function *)
      | R_TUPLE of value list                   (* tuple construction; note that
                                                 * singleton tuples are allowed.
                                                 *)
      | R_SELECT of int * var                   (* tuple selection; zero-based index *)
      | R_DCON of dcon * value list             (* data-constructor application *)

    and pat
      = P_DCON of dcon * var list               (* data constructor application pattern *)
      | P_VAR of var                            (* simple (i.e., variable) pattern *)

    datatype program = PROG of var * exp        (* a program is parameterized by
                                                 * the command-line arguments.
                                                 *)

    (* smart constructors for expressions *)
    local
      fun mkExp mk arg = E(ProgPt.new(), mk arg)
    in
    val mkLET = mkExp E_LET
    fun mkSELECT (x, i, y, e) = mkLET(x, R_SELECT(i, y), e)
    fun mkTUPLE (x, vs, e) = mkLET(x, R_TUPLE vs, e)
    val mkFUN = mkExp E_FUN
    val mkIF = mkExp E_IF
    val mkAPPLY = mkExp E_APPLY
    val mkCASE = mkExp E_CASE
    val mkRET = mkExp E_RET
    end (* local *)

  end
