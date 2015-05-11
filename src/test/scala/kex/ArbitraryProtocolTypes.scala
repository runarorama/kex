package extprot
package compiler

import org.scalacheck._
import Prop._
import Gen._
import Arbitrary.arbitrary
import ProtocolTypes._

import bound.scalacheck.BoundArbitraryInstances._

object ArbitraryProtocolTypes {

  val genTypeOptions: Gen[TypeOptions] = Gen.const(noops)

  val genSimpleBaseTypeExpr: Gen[SimpleBaseTypeExpr] =
    oneOf(BoolT(Map()), ByteT(Map()), IntT(Map()), LongT(Map()), FloatT(Map()), StringT(Map()))

  implicit val arbitrary1CoreBaseTypeExpr: Arbitrary1[CoreBaseTypeExpr] = new Arbitrary1[CoreBaseTypeExpr] {
    def arbitrary1[V](implicit a: Arbitrary[V]): Arbitrary[CoreBaseTypeExpr[V]] = implicitly[Arbitrary[CoreBaseTypeExpr[V]]]
  }

  implicit def arbitraryCoreBaseTypeExpr[V](implicit av: Arbitrary[V]): Arbitrary[CoreBaseTypeExpr[V]] = 
  	Arbitrary(oneOf(
  	  for { t  <- genSimpleBaseTypeExpr } yield SimpleT[V](t),
  	  for { vs <- listOf(arbitrary[V])  } yield TupleT(vs, noops),
  	  for { v  <- arbitrary[V]          } yield ListT (v,  noops),
  	  for { v  <- arbitrary[V]          } yield ArrayT(v,  noops)
  	))

}

