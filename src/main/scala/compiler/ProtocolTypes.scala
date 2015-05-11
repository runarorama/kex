package kex
package compiler

import scalaz._
import Scalaz._
import bound._

object ProtocolTypes {
  type TypeOptions = Map[String, String]
  type BaseType = BaseTypeExpr[String]
  type BaseScope[A] = Scope[String, BaseTypeExpr, A]
  type Type = TypeExpr[String]
  type MessageDef = MessageExpr[String]

  val noops: TypeOptions = Map()

  def constantConstructors[A](sum: SumDataType[A]): List[String] =
    sum.constructors.flatMap {
      case Constant(x) => Some(x)
      case _ => None
    }

  def nonConstantConstructors[A](sum: SumDataType[A]): List[(String, NonEmptyList[A])] =
    sum.constructors.flatMap {
      case NonConstant(n, l) => Some(n -> l)
      case _ => None
    }
}

import ProtocolTypes._

sealed trait SimpleBaseTypeExpr {
  def base[A]: BaseTypeExpr[A] = CoreT(SimpleT[BaseTypeExpr[A]](this))
}
case class BoolT(options: TypeOptions) extends SimpleBaseTypeExpr
case class ByteT(options: TypeOptions) extends SimpleBaseTypeExpr
case class IntT(options: TypeOptions) extends SimpleBaseTypeExpr
case class LongT(options: TypeOptions) extends SimpleBaseTypeExpr
case class FloatT(options: TypeOptions) extends SimpleBaseTypeExpr
case class StringT(options: TypeOptions) extends SimpleBaseTypeExpr

sealed trait CoreBaseTypeExpr[A]
case class SimpleT[A](t: SimpleBaseTypeExpr) extends CoreBaseTypeExpr[A]
case class TupleT[A](as: List[A], options: TypeOptions) extends CoreBaseTypeExpr[A]
case class ListT[A](a: A, options: TypeOptions) extends CoreBaseTypeExpr[A]
case class ArrayT[A](a: A, options: TypeOptions) extends CoreBaseTypeExpr[A]

sealed trait BaseTypeExpr[A] {
  def freeVars: List[A] = Traverse[BaseTypeExpr].toList(this)
  def plate[F[_]:Applicative](
    f: BaseTypeExpr[A] => F[BaseTypeExpr[A]]): F[BaseTypeExpr[A]] = this match {
      case CoreT(t) => t match {
        case SimpleT(_) => f(this)
        case TupleT(as, os) => (as traverse f).map(x => CoreT(TupleT(x, os)))
        case ListT(a, os) => f(a).map(x => CoreT(ListT(x, os)))
        case ArrayT(a, os) => f(a).map(x => CoreT(ArrayT(x, os)))
      }
      case AppT(op, args, os) => (args traverse f).map(AppT(op, _, os))
      case ExtAppT(path, op, args, os) =>
        args.traverse(f).map(ExtAppT(path, op, _, os))
      case x => Applicative[F].pure(x)
  }
  def traverse[M[_]:Applicative,B](f: A => M[B]): M[BaseTypeExpr[B]] = this match {
    case CoreT(t) => t match {
      case SimpleT(s) => (CoreT[B](SimpleT(s)):BaseTypeExpr[B]).pure[M]
      case TupleT(as, os) => (as traverse (_ traverse f)).map(x => CoreT(TupleT(x, os)))
      case ListT(bt, os) => (bt traverse f).map(x => CoreT(ListT(x, os)))
      case ArrayT(bt, os) => (bt traverse f).map(x => CoreT(ArrayT(x, os)))
    }
    case AppT(op, args, os) =>
      ((op traverse f) |@| args.traverse(_ traverse f))(AppT(_, _, os))
    case ExtAppT(path, op, args, os) =>
      args.traverse(_ traverse f).map(ExtAppT(path, op, _, os))
    case TypeParamT(p) => f(p).map(TypeParamT(_))
  }
  def flatMap[B](f: A => BaseTypeExpr[B]): BaseTypeExpr[B] = this match {
    case CoreT(t) => CoreT[B](t match {
      case SimpleT(s) => SimpleT(s)
      case TupleT(as, os) => TupleT(as map (_ flatMap f), os)
      case ListT(a, os) => ListT(a flatMap f, os)
      case ArrayT(a, os) => ArrayT(a flatMap f, os)
    })
    case AppT(op, args, os) => AppT(op flatMap f, args map (_ flatMap f), os)
    case ExtAppT(path, name, exprs, os) =>
      ExtAppT(path, name, exprs.map(_ flatMap f), os)
    case TypeParamT(p) => f(p)
  }
}
case class CoreT[A](t: CoreBaseTypeExpr[BaseTypeExpr[A]]) extends BaseTypeExpr[A]
case class AppT[A](operator: BaseTypeExpr[A],
                   operands: List[BaseTypeExpr[A]],
                   options: TypeOptions) extends BaseTypeExpr[A]
case class ExtAppT[A](path: List[String],
                      name: String,
                      exprs: List[BaseTypeExpr[A]],
                      options: TypeOptions) extends BaseTypeExpr[A]
case class TypeParamT[A](p: A) extends BaseTypeExpr[A]
object BaseTypeExpr {
  implicit val baseTypeExprInstance: Traverse[BaseTypeExpr] with Monad[BaseTypeExpr] =
    new Traverse[BaseTypeExpr] with Monad[BaseTypeExpr] {
      def traverseImpl[G[_]:Applicative,A,B](
        fa: BaseTypeExpr[A])(f: A => G[B]): G[BaseTypeExpr[B]] = fa traverse f
      def point[A](a: => A) = TypeParamT(a)
      def bind[A,B](ma: BaseTypeExpr[A])(f: A => BaseTypeExpr[B]) = ma flatMap f
    }
}

sealed trait TypeExpr[A] {
  def freeVars: List[A] = Traverse[TypeExpr].toList(this)
  def traverse[M[_]:Applicative,B](f: A => M[B]): M[TypeExpr[B]] = this match {
    case BaseT(t) => (t.traverse(f)).map(BaseT(_))
    case RecordT(r, os) => (r traverse (_ traverse f)).map(RecordT(_, os))
    case SumT(s, os) => (s traverse (_ traverse f)).map(SumT(_, os))
  }
}
case class BaseT[A](t: BaseScope[A]) extends TypeExpr[A]
case class RecordT[A](record: RecordDataType[BaseScope[A]],
                      options: TypeOptions) extends TypeExpr[A]
case class SumT[A](sum: SumDataType[BaseScope[A]],
                   options: TypeOptions) extends TypeExpr[A]

case class Field[A](name: String, mutable: Boolean, tpe: A) {
  def traverse[M[_]:Applicative,B](f: A => M[B]): M[Field[B]] =
    f(tpe).map(Field(name, mutable, _))
}
object Field {
  implicit val fieldInstance: Traverse[Field] = new Traverse[Field] {
    def traverseImpl[G[_]:Applicative,A,B](
      fa: Field[A])(f: A => G[B]): G[Field[B]] = fa traverse f
  }
}
case class RecordDataType[A](name: String, fields: List[Field[A]]) {
  def traverse[M[_]:Applicative,B](f: A => M[B]): M[RecordDataType[B]] =
    fields.traverse(_ traverse f).map(RecordDataType(name, _))
}
case class SumDataType[A](name: String, constructors: List[DataConstructor[A]]) {
  def traverse[M[_]:Applicative,B](f: A => M[B]): M[SumDataType[B]] =
    constructors.traverse(_ traverse f).map(SumDataType(name, _))
}
object TypeExpr {
  implicit val typeExprInstance: Traverse[TypeExpr] =
    new Traverse[TypeExpr] {
      def traverseImpl[G[_]:Applicative,A,B](
        fa: TypeExpr[A])(f: A => G[B]): G[TypeExpr[B]] = fa traverse f
    }
}

sealed trait DataConstructor[A] {
  def traverse[M[_]:Applicative,B](f: A => M[B]): M[DataConstructor[B]] = this match {
    case NonConstant(name, args) => args.traverse(f).map(NonConstant(name, _))
    case Constant(name) => Applicative[M].pure(Constant(name))
  }
}
case class Constant[A](name: String) extends DataConstructor[A]
case class NonConstant[A](name: String, args: NonEmptyList[A]) extends DataConstructor[A]
object DataConstructor {
  implicit val dataconstructorInstance: Traverse[DataConstructor] =
    new Traverse[DataConstructor] {
      def traverseImpl[G[_]:Applicative,A,B](
        fa: DataConstructor[A])(f: A => G[B]): G[DataConstructor[B]] = fa traverse f
    }
}

// Message declarations are monomorphic, so we don't need Bound variables here
sealed trait MessageExpr[A] {
  def freeVars: List[A] = Traverse[MessageExpr].toList(this)
  def traverse[M[_]:Applicative,B](f: A => M[B]): M[MessageExpr[B]] = this match {
    case RecordM(fields) => fields.traverse(_ traverse (_ traverse f)).map(RecordM(_))
    case AppM(name, ts, os) => ts.traverse(_ traverse f).map(AppM(name, _, os))
    case MessageAlias(path, alias) => Applicative[M].pure(MessageAlias(path, alias))
    case SumM(sum) => sum.traverse(_ traverse (_ traverse f)).map(SumM(_))
  }
}
case class RecordM[A](fields: List[Field[BaseTypeExpr[A]]]) extends MessageExpr[A]
case class AppM[A](name: String,
                   types: List[BaseTypeExpr[A]],
                   option: TypeOptions) extends MessageExpr[A]
case class MessageAlias[A](names: List[String], alias: String) extends MessageExpr[A]
case class SumM[A](sum: List[(String, MessageExpr[A])]) extends MessageExpr[A]
object MessageExpr {
  implicit val baseTypeTraverse: Traverse[MessageExpr] = new Traverse[MessageExpr] {
    def traverseImpl[G[_]:Applicative,A,B](
      fa: MessageExpr[A])(f: A => G[B]): G[MessageExpr[B]] = fa traverse f
  }
}

sealed trait Declaration {
  def name: String
  def arity: Int
  def freeTypeVariables: List[String]
}
case class MessageDecl(name: String,
                       message: MessageExpr[String],
                       options: TypeOptions) extends Declaration {
  def arity = 0
  def freeTypeVariables: List[String] =
    message.freeVars.filterNot(_ == name)
}
case class TypeDecl(name: String,
                    params: List[String],
                    body: TypeExpr[String],
                    options: TypeOptions) extends Declaration {
  def arity = params.length
  def freeTypeVariables: List[String] =
    body.freeVars.toList.filterNot((name :: params) contains _)
}

