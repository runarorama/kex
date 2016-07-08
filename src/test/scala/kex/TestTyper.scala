package kex

import io.Source._
import org.scalacheck._
import compiler.{Parser, Typer}
import atto._
import Atto._
import ParseResult._

object TestTyper {
  def go = {
    TestParser.parseFile("/type_errors.kex") match {
      case Some(decls) =>
        val errors = Typer.checkDeclarations(decls)
        // The file should have 8 type errors
        errors.size == 8
      case None => false
    }
  }
}

