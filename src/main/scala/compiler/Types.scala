package extprot
package compiler

object ProtocolTypes {
  type TypeOptions = Map[String, String]

  val noops: TypeOptions = Map()

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
case class LongT(options: TypeOptions) extends SimpleBaseTypeExpr
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
  def freeVars(known: List[String]): List[String]
}
case class CoreT(t: CoreBaseTypeExpr[BaseTypeExpr]) extends BaseTypeExpr {
  def freeVars(known: List[String]): List[String] = t match {
    case TupleT(as, _) => as flatMap (_ freeVars known)
    case ListT(a, _) => a freeVars known
    case ArrayT(a, _) => a freeVars known
    case _ => Nil
  }
}
case class AppT(name: String,
                types: List[BaseTypeExpr],
                options: TypeOptions) extends BaseTypeExpr {
  def freeVars(known: List[String]): List[String] = {
    val ts = types flatMap (_ freeVars known)
    if (known contains name) ts else name :: ts
  }
}
case class ExtAppT(path: List[String],
                   name: String,
                   exprs: List[BaseTypeExpr],
                   options: TypeOptions) extends BaseTypeExpr {
  def freeVars(known: List[String]): List[String] =
    exprs flatMap (_ freeVars known)
}
case class TypeParamT(p: TypeParam.Param) extends BaseTypeExpr {
  def freeVars(known: List[String]): List[String] = {
    val s = TypeParam toString p
    if (known contains s) Nil else List(s)
  }
}

sealed trait TypeExpr {
  def freeVars(known: List[String]): List[String]
}
case class BaseT(t: BaseTypeExpr) extends TypeExpr {
  def freeVars(known: List[String]): List[String] = t freeVars known
}
case class RecordT(record: RecordDataType[BaseTypeExpr],
                   options: TypeOptions) extends TypeExpr {
  def freeVars(known: List[String]): List[String] =
    record.fields flatMap {
      case Field(_, _, ty) => ty freeVars known
    }
}
case class SumT(sum: SumDataType[BaseTypeExpr], options: TypeOptions) extends TypeExpr {
  def freeVars(known: List[String]): List[String] =
    sum.constructors flatMap {
      case NonConstant(_, as) => as flatMap (_ freeVars known)
      case _ => Nil
    }
}

case class Field[A](name: String, mutable: Boolean, tpe: A)
case class RecordDataType[A](name: String, fields: List[Field[A]])
case class SumDataType[A](name: String, constructors: List[DataConstructor[A]])

sealed trait DataConstructor[A]
case class Constant[A](name: String) extends DataConstructor[A]
case class NonConstant[A](name: String, args: List[A]) extends DataConstructor[A]

sealed trait MessageExpr {
  def freeVars(known: List[String]): List[String]
}
case class RecordM(fields: List[Field[BaseTypeExpr]]) extends MessageExpr {
  def freeVars(known: List[String]): List[String] =
    fields flatMap {
      case Field(_, _, e) => e freeVars known
    }
}
case class AppM(name: String,
                types: List[BaseTypeExpr],
                option: TypeOptions) extends MessageExpr {
  def freeVars(known: List[String]): List[String] =
    types flatMap { _ freeVars known }
}
case class MessageAlias(names: List[String], alias: String) extends MessageExpr {
  def freeVars(known: List[String]): List[String] = Nil
}
case class SumM(sum: List[(String, MessageExpr)]) extends MessageExpr {
  def freeVars(known: List[String]): List[String] =
    sum flatMap {
      case (_, e) => e freeVars known
    }
}

sealed trait Declaration {
  def name: String
  def arity: Int
  def freeTypeVariables: List[String]
}
case class MessageDecl(name: String,
                       message: MessageExpr,
                       options: TypeOptions) extends Declaration {
  def arity = 0
  def freeTypeVariables: List[String] =
    message freeVars List(name)
}
case class TypeDecl(name: String,
                    params: List[TypeParam.Param],
                    tpe: TypeExpr,
                    options: TypeOptions) extends Declaration {
  def arity = params.length
  def freeTypeVariables: List[String] =
    tpe.freeVars(name :: params.map(TypeParam toString _))
}

