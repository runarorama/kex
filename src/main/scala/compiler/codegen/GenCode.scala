package kex
package compiler
package codegen
import Types._
import kex.compiler.{ProtocolTypes => PT}
import scalaz.std.list._
import scalaz.std.option._
import scalaz.syntax.traverse._

trait GenCode {
  type Container
  type AST

  def generateContainer(bs: Bindings, decl: Declaration): Option[Container]
  def msgDeclGenerators: Map[String, MsgDeclGenerator[Container]]
  def typeDeclGenerators: Map[String, TypeDeclGenerator[Container]]
  def generateCode(containers: List[Container]): AST
}

trait MsgDeclGenerator[C] {
  def apply(bindings: Bindings,
            name: String,
            expr: PT.MessageDef,
            opts: PT.TypeOptions,
            cont: C): C
}

trait TypeDeclGenerator[C] {
  def apply(bindings: Bindings,
            name: String,
            params: List[String],
            expr: PT.Type,
            opts: PT.TypeOptions,
            cont: C): C
}

object GenScala extends MacrosCompatibility {

  case class Generator(ctx: Context) extends GenCode {

    import ctx.universe.{Constant => _, _}

    type AST = ctx.Tree

  def generateContainer(bs: Bindings, decl: Declaration): Option[Container] = ???
  def msgDeclGenerators: Map[String, MsgDeclGenerator[Container]] = ???
  def typeDeclGenerators: Map[String, TypeDeclGenerator[Container]] = ???
  def generateCode(containers: List[Container]): AST = ???

    case class ScalaContainer(
      name: String,
      importModules: Option[AST],
      types: Option[AST],
      reader: Option[AST],
      ioReader: Option[AST],
      prettyPrinter: Option[AST],
      writer: Option[AST],
      defaultFunction: Option[AST]
    )

    type Container = ScalaContainer

    def emptyContainer(name: String, func: Option[AST], types: AST) =
      ScalaContainer(name, None, Some(types), None, None, None, None, func)

    def identWithPath(path: List[String], ident: String) = {
      val fullPath = path :+ ident
      path.foldLeft[Tree](q"${Ident(TermName(fullPath.head))}") { (id, p) =>
        q"${id}.${TermName(p)}"
      }
    }

    def defaultValue(v: LowLevel): Option[AST] = v match {
      case Vint(VBool, _) => Some(q"false")
      case Sum(xs, _) => xs.flatMap {
        case Constant(x) =>
          Some(q"${ctx.parse(x.tpe.capitalize)}.${TermName(x.name)}")
        case _ => None
      }.headOption
      case Htuple(HList, _, _) => Some(q"List()")
      case Htuple(HArray, _, _) => Some(q"Array()")
      case LLMessage(path, name, _) =>
        val fullPath = path ++ List(name.capitalize)
        val id = identWithPath(fullPath, s"${name}_default")
        Some(q"$id")
      case Tuple(tys, _) => tys.traverse(defaultValue).map {
        case Nil      => sys.error("defaultValue: empty tuple")
        case _ :: Nil => sys.error("defaultValue: tuple with only 1 element")
        case x :: xs  => q"($x, ..$xs)"
      }
      case _ => None
    }

    def lookupOption(name: String, opts: PT.TypeOptions, global: Boolean = false): Option[String] =
      if (global) opts.get(name) else opts.get(s"scala.$name")

    def parseType(ty: String) = {
      val q"type T = $t" = ctx.parse(s"type T = $ty")
      t
    }

    def parseExpr(exp: String) = {
      val q"val x = $e" = ctx.parse(s"val x = $exp")
      e
    }

    import java.util.regex.Pattern.quote

    def getTypeInfo(opts: PT.TypeOptions): Option[(ctx.Tree, ctx.Tree, ctx.Tree)] =
      lookupOption("type", opts).flatMap {
        v => v.split(",").map(_.trim) match {
          case Array(ty, from, to) =>
            Some((parseType(ty), parseExpr(from), parseExpr(to)))
          case _ => None
        }
      }

    def getType(default: ctx.Tree, opts: PT.TypeOptions): ctx.Tree =
      getTypeInfo(opts).map(_._1).getOrElse(default)

    def generateContainer(bindings: Bindings): ScalaContainer = {
      def typeDecl(name: String, ctyp: ctx.Tree, params: List[String] = List()): ctx.Tree = {
        val t = TypeName(name)
        val ps = params.map { p =>
          val q"type dummy[$x] = Int" = q"type dummy[${TypeName(p)}] = Int"
          x
        }
        q"type $t[..$ps] = $ctyp"
      }
      ???
    }
  }
}

case class BadOption(msg: String) extends Exception

object GenCode {
  def collectBindings(decls: List[Declaration]): Bindings =
    decls.map(d => d.name -> d).toMap
}

//
//   import Types._
//   val P = ProtocolTypes
//
//   def generateScala(c: Context)(decls: List[Declaration]) = {
//     val G = new GenScala.Generator(c)
//     import G._
//     val bindings = collectBindings(decls)
//     decls.flatMap { decl =>
//       generateContainer(bindings).map { cont =>
//         decl match {
//           case TypeDecl(name, params, expr, opts) => ???
//             //typeDeclGenerators.foldLeft
//         }
//       }
//     }
//   }
//
//   /*def updateBindings(bindings: Bindings,
//                      params: List[String],
//                      args: List[LowLevel]): Bindings =
//     (params, args).zipped.foldLeft(bindings)(_ + _)
//   */
//   /*
//
//   def betaReduceTexpr(bindings: Bindings, texpr: P.BaseType): ReducedType = {
//     def aux(bindings: Bindings, x: P.BaseType): ReducedType =
//     betaReduceTexprAux(aux, betaReduceTexpr, bindings, texpr)
//   }
//
//   def betaReduceTexprAux(f: (Bindings, P.BaseType) => ReducedType,
//                          self: Bindings => P.BaseType => ReducedType,
//                          bindings: Bindings,
//                          expr: P.BaseType): ReducedType = expr match {
//     case CoreT(x@SimpleT(e)) =>
//     case CoreT(TupleT(l, opts)) => CoreT(TupleT(l.map(self(bindings)), opts))
//     case CoreT(ListT(t, opts)) => CoreT(ListT(self(bindings)(t), opts))
//     case CoreT(ArrayT(t, opts)) => CoreT(ArrayT(self(bindings)(t), opts))
//     case x => f(bindings, x)
//   }
//
//   def betaReduceSum(self: Bindings => P.BaseType => ReducedType,
//                     bindings: Bindings,
//                     s: SumDataType[P.BaseType],
//                     opts: P.TypeOptions): ReducedType = {
//     val constructors = s.constructors.map {
//       case x@compiler.Constant(_) => x
//       case compiler.NonConstant(const, tys) =>
//         compiler.NonConstant(const, tys.map(self(bindings)))
//     }
//     SumRT(s copy (constructors = constructors), opts)
//   }
//
//   def betaReduceRecord(self: Bindings => P.BaseType => ReducedType,
//                        bindings: Bindings,
//                        r: RecordDataType[P.BaseType],
//                        opts: P.TypeOptions) = {
//     val fields = r.fields.map {
//       case Field(name, mutable, ty) => Field(name, mutable, self(bindings)(ty))
//     }
//     RecordRT(r copy (fields = fields), opts)
//   }
//
//   def betaReduceTexpr(bindings: Bindings, texpr: P.BaseType): ReducedType = {
//     def aux(bindings: Bindings, x: TypeExpr): ReducedType = {
//       def reduceTexpr(bindings: Bindings, expr: TypeExpr): P.BaseType = expr match {
//         case BaseT(e) => betaReduceTexpr(bindings, e)
//         case x => aux(bindings, x)
//       }
//       x match {
//         case SumT(s, opts) => right(betaReduceSum(betaReduceTexpr, bindings, s, opts))
//         case RecordT(r, opts) => right(betaReduceRecord(betaReduceTexpr, bindings, r, opts))
//         case BaseT(TypeParamT(p)) =>
//           val name = p.toString
//           (bindings get name) match {
//             case Some(MessageDecl(name, _, opts)) => right(MessageRT(name, opts))
//             case Some(TypeDecl(_, Nil, exp, opts)) =>
//               right(reduceTexpr(bindings, exp) mergeOptions opts)
//             case Some(TypeDecl(_, _, _, _)) =>
//               left(s"Wrong arity for higher-order type $name.")
//             case None => left(s"Unbound type variable $name.")
//           }
//         case BaseT(ExtAppT(path, name, Nil, opts)) => right(ExtMessageRT(path, name, opts))
//         case BaseT(ExtAppT(_, _, _, _)) => left("External polymorphic types not supported")
//         case BaseT(AppT(name, args, opts)) => bindings.get(name) match {
//           case Some(MessageDecl(_, _, opts2)) => right(MessageRT(name, opts2 ++ opts))
//           case Some(TypeDecl(_, params, exp, opts2)) =>
//             val os = opts2 ++ opts
//             val bs = updateBindings(bindings,
//                                     params map (Params.toString(_)),
//                                     args map (ty => TypeDecl("bogus", Nil, ty.tpe)))
//             val b = ???
//             ???
//         }
//       }
//     }
//   }*/
// }
