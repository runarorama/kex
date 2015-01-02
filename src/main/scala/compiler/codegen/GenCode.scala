package extprot
package compiler
package codegen

object GenCode {
  import Types._
  val P = ProtocolTypes

  /*def updateBindings(bindings: Bindings,
                     params: List[String],
                     args: List[LowLevel]): Bindings =
    (params, args).zipped.foldLeft(bindings)(_ + _)

  def betaReduceTexpr(bindings: Bindings, texpr: P.BaseType): ReducedType = {
    def aux(bindings: Bindings, x: P.BaseType): ReducedType =
    betaReduceTexprAux(aux, betaReduceTexpr, bindings, texpr)
  }

  def betaReduceTexprAux(f: (Bindings, P.BaseType) => ReducedType,
                         self: Bindings => P.BaseType => ReducedType,
                         bindings: Bindings,
                         expr: P.BaseType): ReducedType = expr match {
    case CoreT(x@SimpleT(e)) =>
    case CoreT(TupleT(l, opts)) => CoreT(TupleT(l.map(self(bindings)), opts))
    case CoreT(ListT(t, opts)) => CoreT(ListT(self(bindings)(t), opts))
    case CoreT(ArrayT(t, opts)) => CoreT(ArrayT(self(bindings)(t), opts))
    case x => f(bindings, x)
  }

  def betaReduceSum(self: Bindings => P.BaseType => ReducedType,
                    bindings: Bindings,
                    s: SumDataType[P.BaseType],
                    opts: P.TypeOptions): ReducedType = {
    val constructors = s.constructors.map {
      case x@compiler.Constant(_) => x
      case compiler.NonConstant(const, tys) =>
        compiler.NonConstant(const, tys.map(self(bindings)))
    }
    SumRT(s copy (constructors = constructors), opts)
  }

  def betaReduceRecord(self: Bindings => P.BaseType => ReducedType,
                       bindings: Bindings,
                       r: RecordDataType[P.BaseType],
                       opts: P.TypeOptions) = {
    val fields = r.fields.map {
      case Field(name, mutable, ty) => Field(name, mutable, self(bindings)(ty))
    }
    RecordRT(r copy (fields = fields), opts)
  }

  def betaReduceTexpr(bindings: Bindings, texpr: P.BaseType): ReducedType = {
    def aux(bindings: Bindings, x: TypeExpr): ReducedType = {
      def reduceTexpr(bindings: Bindings, expr: TypeExpr): P.BaseType = expr match {
        case BaseT(e) => betaReduceTexpr(bindings, e)
        case x => aux(bindings, x)
      }
      x match {
        case SumT(s, opts) => right(betaReduceSum(betaReduceTexpr, bindings, s, opts))
        case RecordT(r, opts) => right(betaReduceRecord(betaReduceTexpr, bindings, r, opts))
        case BaseT(TypeParamT(p)) =>
          val name = p.toString
          (bindings get name) match {
            case Some(MessageDecl(name, _, opts)) => right(MessageRT(name, opts))
            case Some(TypeDecl(_, Nil, exp, opts)) =>
              right(reduceTexpr(bindings, exp) mergeOptions opts)
            case Some(TypeDecl(_, _, _, _)) =>
              left(s"Wrong arity for higher-order type $name.")
            case None => left(s"Unbound type variable $name.")
          }
        case BaseT(ExtAppT(path, name, Nil, opts)) => right(ExtMessageRT(path, name, opts))
        case BaseT(ExtAppT(_, _, _, _)) => left("External polymorphic types not supported")
        case BaseT(AppT(name, args, opts)) => bindings.get(name) match {
          case Some(MessageDecl(_, _, opts2)) => right(MessageRT(name, opts2 ++ opts))
          case Some(TypeDecl(_, params, exp, opts2)) =>
            val os = opts2 ++ opts
            val bs = updateBindings(bindings,
                                    params map (Params.toString(_)),
                                    args map (ty => TypeDecl("bogus", Nil, ty.tpe)))
            val b = ???
            ???
        }
      }
    }
  }*/
}
