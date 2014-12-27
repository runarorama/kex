package extprot
package compiler

import ProtocolTypes._

sealed trait TypeError {
  def print: String
}
case class RepeatedBinding(name: String) extends TypeError {
  def print = s"Type binding $name is duplicated."
}
case class UnboundTypeVar(where: String, which: String) extends TypeError {
  def print = s"Type $which is unbound in $where."
}
case class WrongArity(which: String, correct: Int, where: String, wrong: Int) {
  def print = s"type $which used with wrong arity ($correct instead of $wrong) in $where."
}

object Typer {
  def freeTypeVariables(decl: Declaration): List[String] = decl match {
    case MessageDecl(name, m, _) => msgFreeVars(List(name), m)
  }
}
