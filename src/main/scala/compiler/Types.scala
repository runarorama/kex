package extprot
package compiler

object ProtocolTypes {
  type TypeOptions = Map[String, String]

  def constantConstructors[A](sum: SumDataType[A]): List[String] =
    sum.constructors.flatMap {
      case Constant(x) => Some(x)
      case _ => None
    }

  def nonConstantConstructors[A](sum: SumDataType[A]): List[(String, List[A])] =
    sum.constructors.flatMap {
      case NonConstant(n, l) => Some(n -> l)
      case _ => None
    }
}

import ProtocolTypes._

sealed trait SimpleBaseTypeExpr {
  lazy val base: BaseTypeExpr = CoreT(SimpleT(this))
  lazy val tpe: TypeExpr = BaseT(base)
}
case class BoolT(options: TypeOptions) extends SimpleBaseTypeExpr
case class ByteT(options: TypeOptions) extends SimpleBaseTypeExpr
case class IntT(options: TypeOptions) extends SimpleBaseTypeExpr
case class LongIntT(options: TypeOptions) extends SimpleBaseTypeExpr
case class FloatT(options: TypeOptions) extends SimpleBaseTypeExpr
case class StringT(options: TypeOptions) extends SimpleBaseTypeExpr

sealed trait CoreBaseTypeExpr[A]
case class SimpleT[A](t: SimpleBaseTypeExpr) extends CoreBaseTypeExpr[A]
case class TupleT[A](as: List[A], options: TypeOptions) extends CoreBaseTypeExpr[A]
case class ListT[A](a: A, options: TypeOptions) extends CoreBaseTypeExpr[A]
case class ArrayT[A](a: A, options: TypeOptions) extends CoreBaseTypeExpr[A]

abstract class TypeParam {
  type Param

  def fromString(s: String): Param
  def toString(p: Param): String
  def name(p: Param): String
}

object TypeParam extends TypeParam {
  type Param = String
  def fromString(s: String) = s
  def toString(p: Param) = p
  def name(p: Param) = p
}

sealed trait BaseTypeExpr {
  lazy val tpe: TypeExpr = BaseT(this)
}
case class CoreT(t: CoreBaseTypeExpr[BaseTypeExpr]) extends BaseTypeExpr
case class AppT(s: String, exprs: List[BaseTypeExpr], options: TypeOptions) extends BaseTypeExpr
case class ExtAppT(ss: List[String],
                   s: String,
                   exprs: List[BaseTypeExpr],
                   options: TypeOptions) extends BaseTypeExpr
case class TypeParamT(p: TypeParam) extends BaseTypeExpr

sealed trait TypeExpr
case class BaseT(t: BaseTypeExpr) extends TypeExpr
case class RecordT(record: RecordDataType[BaseTypeExpr], options: TypeOptions) extends TypeExpr
case class SumT(sum: SumDataType[BaseTypeExpr], options: TypeOptions) extends TypeExpr

case class Field[A](name: String, mutable: Boolean, tpe: A)
case class RecordDataType[A](name: String, fields: List[Field[A]])

case class SumDataType[A](name: String, constructors: List[DataConstructor[A]])

sealed trait DataConstructor[A]
case class Constant[A](name: String) extends DataConstructor[A]
case class NonConstant[A](name: String, args: List[A]) extends DataConstructor[A]

sealed trait MessageExpr
case class BaseM(fields: Field[BaseTypeExpr]) extends MessageExpr
case class AppM(name: String,
                types: List[BaseTypeExpr],
                option: TypeOptions) extends MessageExpr
case class MessageAlias(names: List[String], alias: String) extends MessageExpr
case class SumM(sum: List[Either[BaseM, AppM]]) extends MessageExpr

sealed trait Declaration
case class MessageDecl(name: String, message: MessageExpr, options: TypeOptions)
case class TypeDecl(name: String, params: List[TypeParam], tpe: TypeExpr, options: TypeOptions)


