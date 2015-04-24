package extprot

import io.Source._
import org.scalacheck._

object TestParser {
  def go = {

    val files = List("/test.proto", "/simple.proto", "/grafff.proto", "/address_book.proto")

    import compiler.Parser
    import atto._
    import Atto._
    import ParseResult._

    def parseFile(f: String) = try {
      Parser(fromInputStream(this.getClass.getResourceAsStream(f)).mkString) match {
        case Done(input, a) =>
          if (input.size > 0) {
            println(s"Unexpected input: $input")
            false
          }
          else true
        case Fail(input, stack, message) =>
          println(s"Failed parsing $f: $message\nInput was:'$input'\nStack trace: $stack\n")
          false
        case _ =>
          println(s"Unexpected EOF in $f")
          false
      }
    } catch {
      case e: NullPointerException =>
        println(s"File not found: $f")
        false
    }

    files.foldLeft(true)((a, b) => a && parseFile(b))
  }
}
