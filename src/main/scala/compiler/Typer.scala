package kex
package compiler

import ProtocolTypes._
import scalaz._
import Scalaz._
import bound._

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
  type V[A] = ValidationNel[Error, A]
  type S[A] = State[Map[String, Int], V[A]]
  type Check = S[Unit]

  val Va = Validation.ValidationApplicative[NonEmptyList[Error]]
  def Sa[S] = StateT.stateMonad[S]

  val State = MonadState[State, Map[String,Int]]
  import State._

  implicit def checkApplicative[S] =
    Sa[S].compose[V](Va)

  val ok: V[Unit] = ().successNel[Error]
  val okok: Check = state(ok)

  def forget[A,M[_]:Monad](scope: Scope[A,M,A]): M[A] =
    scope.instantiate(x => Monad[M].pure(x))

  def checkDeclarations(decls: List[Declaration]): List[Error] = {
    def dupErrors(decl: Declaration): Check = {
      val name = decl.name
      gets(_.keySet).flatMap { bindings =>
        if (bindings contains name)
          state(RepeatedBinding(name).failureNel)
        else
          modify(_ + (name -> 0)).map(_.success)
      }
    }
    def unboundTypeVars(decl: Declaration): Check =
      decl.freeTypeVariables.traverse_[S] { n =>
        gets(_.keySet).map { bindings =>
          if (bindings contains n) ok
          else UnboundTypeVar(decl.name, n).failureNel
        }
      }
    def wrongTypeArities(decl: Declaration): Check = {
      def checkMsg(msg: MessageDef): Check = msg match {
        case RecordM(r) => r.traverse_[S] {
          case Field(_, _, ty) => checkBaseType(ty)
        }
        case AppM(s, params, _) => checkApp(s, params)
        case SumM(ms) => ms.traverse_(m => checkMsg(m._2))
        case x => okok
      }
      def checkType(ty: Type): Check = ty match {
        case BaseT(scope) => checkBaseType(forget(scope))
        case RecordT(r, _) => r.fields.traverse_ {
          case Field(_, _, scope) => checkBaseType(forget(scope))
        }
        case SumT(sum, _) => sum.constructors.traverse_[S] {
          case NonConstant(_, as) => as.traverse_[S](scope => checkBaseType(forget(scope)))
          case _ => okok
        }
      }
      def checkBaseType(ty: BaseType): Check = {
        def chap(t: BaseType): S[BaseType] = t match {
          // Inline application of types is disallowed by the parser,
          // otherwise we'd need a kind check here
          case sub@AppT(TypeParamT(s), params, _) =>
            checkApp(s, params).map(_.map(_ => sub))
          case x => state(x.success)
        }
        (chap(ty) |@| ty.plate[S](chap))(((x: BaseType), (y: BaseType)) => ())
      }
      def checkApp(v: String, args: List[BaseType]) = {
        get.map { arities =>
          val expected = args.length
          arities.get(v) match {
            case Some(n) if n != expected =>
              WrongArity(v, n, decl.name, expected).failureNel
            case _ => ok
          }
        }
      }
      for {
        _ <- modify(_ + (decl.name -> decl.arity)).map(_.success)
        e <- decl match {
          case MessageDecl(_, msg, _) => checkMsg(msg)
          case TypeDecl(_, _, ty, _) => checkType(ty)
        }
      } yield e
    }
    decls.traverse_[S](d =>
      List(dupErrors _,
           unboundTypeVars _,
           wrongTypeArities _).traverse_[S](_(d))).eval(Map()).fold(_.list, _ => Nil)
  }
}

