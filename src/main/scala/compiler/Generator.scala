package extprot
package compiler

object Core {
  type Tag = Int
  val T = ProtocolTypes
  import T.TypeOptions

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

  case class LLField(name: String, tpe: LowLevel)

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
  case object VInt extends VintMeaning

  sealed trait B64Meaning
  case object BLong extends B64Meaning
  case object BFloat extends B64Meaning

  sealed trait HTupleMeaning
  case object HList extends HTupleMeaning
  case object HArray extends HTupleMeaning

  sealed trait ReducedTypeExpr
  case class ExtMessageRT(path: List[String],
                          name: String,
                          options: TypeOptions) extends ReducedTypeExpr
  case class MessageRT(name: String, options: TypeOptions) extends ReducedTypeExpr
  case class RecordRT(record: RecordDataType[ReducedTypeExpr],
                      options: TypeOptions) extends ReducedTypeExpr
  case class SumRT(sum: SumDataType[ReducedTypeExpr],
                   options: TypeOptions) extends ReducedTypeExpr
  case class BaseRT(base: CoreBaseTypeExpr[ReducedTypeExpr]) extends ReducedTypeExpr

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

  sealed trait PolyTypeExpr
  case class PolyRecord(record: RecordDataType[PolyTypeExprCore],
                        options: TypeOptions) extends PolyTypeExpr
  case class PolySum(sum: SumDataType[PolyTypeExprCore],
                     options: TypeOptions) extends PolyTypeExpr
  case class PolyCore(base: PolyTypeExprCore) extends PolyTypeExpr
}
