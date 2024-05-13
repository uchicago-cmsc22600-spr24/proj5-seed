(* simplify-type.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Support for determining the representation of type and data constructors, as well
 * as translation from AST types to primitive types.  This information is computed
 * by demand.
 *)

structure SimplifyType : sig

    (* information about type-constructor and data-constructor representations *)
    type t

    (* initialize the representation information for the Basis type constructors and
     * data constructors.
     *)
    val initial : unit -> t

    (* get representation info *)
    val tycRepOf : t -> TyCon.t -> PrimType.t
    val conRepOf : t -> DataCon.t -> SimpleDataCon.t

    (* return the list of SimpleAST data constructors for a type constructor (only
     * valid after the type constructor has been analyzed).  The list will have
     * nullary constructors first, followed by constructor functions.
     *)
    val consOf : t -> TyCon.t -> SimpleDataCon.t list

    (* translate an AST type to a primitive type *)
    val cvtTy : t -> Type.ty -> PrimType.t

    (* translate an AST type scheme to a primitive type *)
    val cvtScheme : t -> Type.scheme -> PrimType.t

    (* analyse a user type and its constructors *)
    val analyze : t -> Type.usr_tyc -> unit

  end = struct

    structure B = Basis
    structure DC = DataCon
    structure TycTbl = TyCon.Tbl
    structure ConTbl = DC.Tbl
    structure SDCon = SimpleDataCon
    structure PTy = PrimType

    (* con_rep = Enum of int | Transparent | Box | TaggedBox of int *)
    datatype con_rep = datatype SDCon.con_rep

    datatype t = Info of {
        tycRep : PTy.t TycTbl.hash_table,
        conRep : SDCon.t ConTbl.hash_table
      }

    fun tycRepOf (Info{tycRep, ...}) = TycTbl.lookup tycRep
    fun conRepOf (Info{conRep, ...}) = ConTbl.lookup conRep

    fun initial () = let
          val tycRep = TycTbl.mkTable (32, Fail "TyCon rep table")
          val conRep = ConTbl.mkTable (32, Fail "DataCon rep table")
          val insTycRep = TycTbl.insert tycRep
          val insConRep = ConTbl.insert conRep
          in
            List.app insTycRep [
                (B.tycBool, PTy.intTy),
                (B.tycInt, PTy.intTy),
                (B.tycList, PTy.anyTy),
                (B.tycString, PTy.stringTy),
                (B.tycUnit, PTy.unitTy)
              ];
            List.app insConRep [
                (B.conTrue, SimpleBasis.conTrue),
                (B.conFalse, SimpleBasis.conFalse),
                (B.conCons, SimpleBasis.conCons),
                (B.conNil, SimpleBasis.conNil)
              ];
            Info{tycRep = tycRep, conRep = conRep}
          end

    (* get the AST argument type of a data constructor function *)
    fun argTysOf dc = #2 (DC.tySigOf dc)

    (* convert an AST type to a primitive type using the given mapping for
     * type constructors
     *)
    fun cvtType tycRepOf = let
          fun cvt ty = (case Type.prune ty
                 of Type.VarTy _ => PTy.anyTy
                  | Type.MetaTy _ => PTy.anyTy (* unresolved meta type *)
                  | Type.ConTy(Type.FunTyc, _) => PTy.ptrTy
                  | Type.ConTy(Type.TupleTyc _, _) => PTy.ptrTy
                  | Type.ConTy(tyc, _) => tycRepOf tyc
                  | Type.ErrorTy => raise Fail "unexpected ErrorTy"
                (* end case *))
          in
            cvt
          end

    fun consOf info tyc = List.map (conRepOf info) (TyCon.consOf tyc)

    fun cvtTy (Info{tycRep, ...}) = cvtType (TycTbl.lookup tycRep)

    fun cvtScheme info = let
          val cvt = cvtTy info
          in
            fn (Type.TyScm(_, ty)) => cvt ty
          end

    fun analyze (info as Info{tycRep, conRep}) dt = let
          val tyc = Type.UserTyc dt
          val insTycRep = TycTbl.insert tycRep
          val tycRepOf = TycTbl.lookup tycRep
          val insConRep = ConTbl.insert conRep
          fun mkCon (dc, rep) = insConRep (dc, SDCon.new(DC.nameOf dc, [], rep))
          fun mkConFn (dc, argTys, rep) =
                insConRep (dc, SDCon.new(DC.nameOf dc, argTys, rep))
          val cvtTy = cvtType tycRepOf
          (* convert the argument type of a data-constructor function *)
          fun cvtArgTys dc = (case argTysOf dc
                 of [] => raise Fail "impossible: not a function"
                  | tys => List.map cvtTy tys
                (* end case *))
          (* assign enumeration values for nullary data constructors *)
          fun assignEnumTags ubDCs =
                List.appi (fn (i, dc) => mkCon(dc, Enum i)) ubDCs
          (* assign enumeration values for data-constructor functions *)
          fun assignBoxTags bDCs = let
                fun mkCon (i, dc) = let
                      val tys = cvtArgTys dc
                      in
                        mkConFn (dc, tys, TaggedTuple{tag=i, arity=length tys})
                      end
                in
                  List.appi mkCon bDCs
                end
          in
            (* we initially assign the tyc AnyTy in case `tyc` is recursive *)
            insTycRep (tyc, PTy.anyTy);
            case List.partition DC.isNullary (! (#cons dt))
             of ([], [dc]) => (case cvtArgTys dc
                   of [argTy] => (
                        (* a single constructor with one argument is represented
                         * as its argument
                         *)
                        insTycRep (tyc, argTy);
                        mkConFn (dc, [argTy], Transparent(PTy.kindOf argTy)))
                    | argTys => (
                        insTycRep (tyc, PTy.ptrTy);
                        mkConFn (dc, argTys, Tuple(length argTys)))
                  (* end case *))
              | ([], bDCs) => (
                  (* multiple data-constructor functions are represented as tagged boxes *)
                  insTycRep (tyc, PTy.ptrTy);
                  assignBoxTags bDCs)
              | (ubDCs, []) => (
                  (* only nullary constructors, which are represented as integers *)
                  insTycRep (tyc, PTy.intTy);
                  assignEnumTags ubDCs)
              | (ubDCs, [dc]) => (
                  (* the representation of single data-constructor function
                   * depends on its argument types.
                   *)
                  (* the tyc's representation is already anyTy *)
                  assignEnumTags ubDCs;
                  case cvtArgTys dc
                   of [argTy] => (case PTy.kindOf argTy
                         of PTy.Boxed =>
                              (* we represent the constructor as its argument pointer *)
                              mkConFn (dc, [argTy], Transparent(PTy.Boxed))
                          | _ => (* we need a box around the argument *)
                              mkConFn (dc, [argTy], Tuple 1)
                        (* end case *))
                    | tys => mkConFn (dc, tys, Tuple(length tys))
                  (* end case *))
              | (ubDCs, bDCs) => (
                  (* multiple data-constructor functions are represented as tagged boxes *)
                  insTycRep (tyc, PTy.anyTy);
                  assignEnumTags ubDCs;
                  assignBoxTags bDCs)
            (* end case *)
          end

  end
