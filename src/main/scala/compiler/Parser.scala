package extprot
package compiler

trait Constructor
case class Constant(s: String) extends Constructor
case class NonConstant(s: String, t: List[BaseTypeExpr])

object Parser {
  import scalaparsers.Document._
  import scalaparsers.Diagnostic._

  val P = new scalaparsers.Parsing[Unit] {}
  import P._

  lazy val declarations: Parser[List[Declaration]] = declaration.many

  lazy val messageDecl: Parser[Declaration] = (for {
    _ <- "message"
    name <- lident
    _ <- "="
    e <- msgExpr
    opts <- typeOptions
  } yield MessageDecl(name, e, opts)) scope "message declaration"

  lazy val typeDecl: Parser[Declaration] = for {
    name <- "type" >> lident
    par <- ("'" >> lident.map(TypeParam.fromString)).many
    e <- "=" >> typeExpr
    opts <- typeOptions
  } yield {
    val ee = e match {
      case SumT(s, opts) => SumT(s copy (typeName = name), opts)
      case RecordT(r, opts) => RecordT(r copy (recordName = name), opts)
      case _ => e
    }
    TypeDecl(name, par, e, opts)
  } scope "type declaration"

  lazy val declaration = messageDecl | typeDecl

  lazy val typeOptions =
    ("options" >> typeOptionValues.many).scope("type options") | pure(List())

  lazy val typeExpr =
    "top" >> (constDeclarations sepBy1 "|") map (SumDataType("bogus", _)) scope
  "type expression"

  lazy val lident = ident(lower)
  lazy val uident = ident(upper)

  def ident(p: Parser[Char]): Parser[Name] = (for {
    n <- p.map2(takeWhile(isCont))(_ +: _)
  } yield n.mkString) scope "identifier"

  def isCont(c: Char) = Character.isLetterOrDigit(c) || c == '_' || c == '-'

  def takeWhile(p: Char => Boolean): Parser[List[Char]] = satisfy(p).many
}
