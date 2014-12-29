package extprot
package compiler

import ProtocolTypes._

sealed trait Error {
  def print: String
}
case class RepeatedBinding(name: String) extends Error {
  def print = s"Type binding $name is duplicated."
}
case class UnboundTypeVar(where: String, which: String) extends Error {
  def print = s"Type $which is unbound in $where."
}
case class WrongArity(which: String, correct: Int, where: String, wrong: Int) extends Error {
  def print = s"type $which used with wrong arity ($correct instead of $wrong) in $where."
}

object Typer {
  def checkDeclarations(decls: List[Declaration]): List[Error] = {
    def dupErrors(xs: List[Declaration]) = {
      def loop(errs: List[Error],
               bindings: Set[String],
               ds: List[Declaration]): List[Error] = ds match {
        case Nil => errs
        case decl :: tl =>
          val name = decl.name
          if (bindings contains name)
            loop(RepeatedBinding(name) :: errs, bindings, tl)
          else
            loop(errs, bindings + name, tl)
      }
      loop(Nil, Set(), xs)
    }
    def unboundTypeVars(ds: List[Declaration]) = {
      def loop(es: List[Error],
               bindings: Set[String],
               ds: List[Declaration]): List[Error] = ds match {
        case Nil => es
        case decl :: tl =>
          val name = decl.name
          val errs = decl.freeTypeVariables.foldRight(es) { (n, l) =>
            if (bindings contains n) l else UnboundTypeVar(name, n) :: l
          }
          loop(errs, bindings + decl.name, tl)
      }
      loop(Nil, Set(), ds)
    }
    def wrongTypeArities(ds: List[Declaration]) = {
      def loop(errs: List[Error],
               as: Map[String, Int],
               ds: List[Declaration]): List[Error] = ds match {
        case Nil => errs
        case decl :: tl =>
          val name = decl.name
          val arities = as + (name -> decl.arity)
          def foldApp(acc: List[Error], s: String, params: List[BaseTypeExpr]): List[Error] = {
            val expected = params.length
            val es = arities.get(s) match {
              case None => acc
              case Some(n) if n == expected => acc
              case Some(n) => List(WrongArity(s, n, name, expected))
            }
            params.foldLeft(es)(foldBaseType)
          }
          def foldBaseType(acc: List[Error], t: BaseTypeExpr): List[Error] = t match {
            case AppT(s, params, _) => foldApp(acc, s, params)
            case ExtAppT(_, _, tys, _) => tys.foldLeft(acc)(foldBaseType)
            case CoreT(ListT(t, _)) => foldBaseType(acc, t)
            case CoreT(ArrayT(t, _)) => foldBaseType(acc, t)
            case CoreT(TupleT(l, _)) => l.foldLeft(acc)(foldBaseType)
            case _ => acc
          }
          def foldMsg(acc: List[Error], e: MessageExpr): List[Error] = e match {
            case RecordM(r) => r.foldLeft(acc) {
              case (errs, Field(_, _, ty)) => foldBaseType(errs, ty)
            }
            case MessageAlias(_, _) => acc
            case AppM(s, params, _) => foldApp(acc, s, params)
            case SumM(l) => l.foldLeft(acc) {
              case (errs, (_, msg)) => foldMsg(errs, msg)
            }
          }
          def foldType(acc: List[Error], ty: TypeExpr): List[Error] = ty match {
            case BaseT(bty) => foldBaseType(acc, bty)
            case RecordT(r, _) => r.fields.foldLeft(acc) {
              case (errs, Field(_, _, ty)) => foldBaseType(errs, ty)
            }
            case SumT(sum, _) => sum.constructors.foldLeft(acc) {
              case (errs, NonConstant(_, as)) =>
                as.foldLeft(errs)(foldBaseType)
              case (errs, _) => errs
            }
          }
          decl match {
            case MessageDecl(_, msg, _) => loop(foldMsg(errs, msg), arities, tl)
            case TypeDecl(_, _, ty, _) => loop(foldType(errs, ty), arities, tl)
          }
      }
      loop(Nil, Map(), ds)
    }
    dupErrors(decls) ++ unboundTypeVars(decls) ++ wrongTypeArities(decls)
  }
}

