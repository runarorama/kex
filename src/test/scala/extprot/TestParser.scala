package extprot

import io.Source._
import org.scalacheck._
import compiler.{Parser, Typer, Declaration}
import atto._
import Atto._
import ParseResult._

object TestParser {

  def parseFile(f: String): Option[List[Declaration]] = try {
    Parser(fromInputStream(this.getClass.getResourceAsStream(f)).mkString) match {
      case Done(input, a) =>
        if (input.size > 0) {
          println(s"Unexpected input: $input")
          None
        }
        else Some(a)
      case Fail(input, stack, message) =>
        println(s"Failed parsing $f: $message\nInput was:'$input'\nStack trace: $stack\n")
        None
      case _ =>
        println(s"Unexpected EOF in $f")
        None
    }
  } catch {
    case e: NullPointerException =>
      println(s"File not found: $f")
      None
  }

  def typeCheck(decls: List[Declaration]): Boolean =
    Typer.checkDeclarations(decls).isEmpty

  def go = {

    val files = List("/test.proto", "/simple.proto", "/grafff.proto", "/address_book.proto", "/test_types.proto")

    files.foldLeft(true)((a, b) => a && parseFile(b).map(typeCheck).getOrElse(false))
  }
}
