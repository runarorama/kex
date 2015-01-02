package extprot
package compiler
package codegen

import ProtocolTypes.TypeOptions

object Types {
  type Tag = Int
  type Bindings = Map[String, LowLevel]
}

import Types._

sealed trait LowLevel
case class Vint(meaning: VintMeaning, options: TypeOptions) extends LowLevel
case class Bitstring32(options: TypeOptions) extends LowLevel
case class Bitstring64(meaning: B64Meaning, options: TypeOptions) extends LowLevel
case class Bytes(options: TypeOptions) extends LowLevel
case class Sum(cases: List[Case], options: TypeOptions) extends LowLevel
case class Record(name: String, fields: List[LLField], options: TypeOptions) extends LowLevel
case class Tuple(elements: List[LowLevel], options: TypeOptions) extends LowLevel
case class Htuple(meaning: HTupleMeaning,
                  value: LowLevel,
                  options: TypeOptions) extends LowLevel
case class LLMessage(path: List[String],
                     name: String,
                     options: TypeOptions) extends LowLevel

case class Constructor(tag: Tag, name: String, tpe: String)

sealed trait Case
case class Constant(constructor: Constructor) extends Case
case class NonConstant(constructor: Constructor, args: List[LowLevel]) extends Case

case class LLField(name: String, value: LowLevel)

sealed trait Message[A]
case class MessageSingle[A](namespace: Option[String],
                            fields: List[Field[A]]) extends Message[A]
case class MessageSum[A](namespace: Option[String],
                         constructor: String,
                         fields: List[Field[A]]) extends Message[A]
case class MessageAlias[A](path: List[String], name: String) extends Message[A]

sealed trait VintMeaning
case object VBool extends VintMeaning
case object VInt8 extends VintMeaning
case object VInt32 extends VintMeaning

sealed trait B64Meaning
case object BLong extends B64Meaning
case object BFloat extends B64Meaning

sealed trait HTupleMeaning
case object HList extends HTupleMeaning
case object HArray extends HTupleMeaning

// A monomorphic type expression in Beta normal form,
// having no type applications or type variables
sealed trait ReducedType
case class ExtMessageRT(path: List[String],
                        name: String,
                        options: TypeOptions) extends ReducedType
case class MessageRT(name: String, options: TypeOptions) extends ReducedType
case class RecordRT(record: RecordDataType[ReducedType],
                    options: TypeOptions) extends ReducedType
case class SumRT(sum: SumDataType[ReducedType],
                 options: TypeOptions) extends ReducedType
case class BaseRT(base: CoreBaseTypeExpr[ReducedType],
                  options: TypeOptions) extends ReducedType

// A polymorphic type
sealed trait PolyTypeExpr
case class PolyRecord(record: RecordDataType[PolyTypeExprCore],
                      options: TypeOptions) extends PolyTypeExpr
case class PolySum(sum: SumDataType[PolyTypeExprCore],
                   options: TypeOptions) extends PolyTypeExpr
case class PolyCore(base: PolyTypeExprCore) extends PolyTypeExpr

sealed trait PolyTypeExprCore
case class TypeArg(name: String) extends PolyTypeExprCore
case class ExtType(names: List[String],
                   name: String,
                   exprs: List[PolyTypeExprCore],
                   options: TypeOptions) extends PolyTypeExprCore
case class PolyType(name: String,
                    args: List[PolyTypeExprCore],
                    options: TypeOptions) extends PolyTypeExprCore
case class BasePoly(base: CoreBaseTypeExpr[PolyTypeExprCore]) extends PolyTypeExprCore

