package extprot
package compiler

import scalaz._
import bound._
import atto._
import Atto._
import atto.Parser._
import scalaz.syntax.monad._

object Parser {
  import ProtocolTypes._

  def apply(s: String): ParseResult[List[Declaration]] = parse(declarations, s)

  implicit class Scope[A](p: Parser[A]) {
    def |:(s: String): Parser[A] = p named s
  }

  // Skip lines, comments, or horizontal white space
  lazy val skipLWS: Parser[Unit] = (newline | whitespace).skipMany

  // Skip comments or horizontal whitespace
  lazy val skipHWS: Parser[Unit] = whitespace.skipMany

  lazy val declarations: Parser[List[Declaration]] = declaration.many

  lazy val messageDecl: Parser[Declaration] = "message declaration" |: {
    for {
      name <- string("message") *> lident <* string("=")
      e <- msgExpr
      opts <- typeOptions
    } yield MessageDecl(name, e, opts)
  }

  lazy val typeDecl: Parser[Declaration] = "type declaration" |: {
    for {
      name <- string("type") *> lident
      par <- string("'") *> lident.many
      e <- string("=") *> typeExpr(name, par)
      opts <- typeOptions
    } yield {
      val ee = e match {
        case SumT(s, opts) => SumT(s copy (name = name), opts)
        case RecordT(r, opts) => RecordT(r copy (name = name), opts)
        case _ => e
      }
      TypeDecl(name, par, ee, opts)
    }
  }

  lazy val declaration: Parser[Declaration] =
    skipLWS *> (messageDecl | typeDecl)

  lazy val typeOptions: Parser[TypeOptions] = "type options" |: {
    (string("options") *> typeOptionValues.many.map(_.toMap)) | ok(noops)
  }

  lazy val newline: Parser[Unit] =
    skip(c => c == '\r' || c == '\n') named "newline"

  lazy val whitespace: Parser[Unit] =
    skip(c => c.isWhitespace && c != '\r' && c != '\n') named "whitespace"

  lazy val typeOptionValues =
    (stringLiteral <* string("=")) ~ stringLiteral

  def toScope(args: List[String])(t: BaseType): BaseScope[String] =
    abstrakt[BaseTypeExpr, String, String](t)(x => args find (a => a == x))

  def typeExpr(name: String, args: List[String]): Parser[Type] = (s"top-level type $name" |:(
    constDeclarations.sepBy1(string("|")).map(
      cs => SumT(SumDataType(name, cs.map(_.map(toScope(args))).list), noops)) |
    recordType(name, args)
  )) | (s"simple type $name" |: typeExprSimple.map(t => BaseT(toScope(args)(t))))

  lazy val constDeclarations: Parser[DataConstructor[BaseType]] =
    "data constructor declaration" |: {
      (uident |@| constParams)(NonConstant[BaseType](_,_)) |
      uident.map(Constant[BaseType](_))
    }

  lazy val constParams: Parser[NonEmptyList[BaseType]] =
    typeExprSimple.many1

  lazy val typeExprSimple: Parser[BaseType] =
    squareBrackets(typeExprSimple).map(x => CoreT(ListT(x, noops))) |
    envelopes(typeExprSimple).map(x => CoreT(ArrayT(x, noops))) |
    tuple |
    (for {
      n <- lident <* string("<")
      targs <- typeExprSimple.sepBy1(string(",")) <* string(">")
    } yield AppT(TypeParamT(n), targs.list, noops)) |
    (string("'") *> lident.map(TypeParamT(_))) |
    lident.map(n => AppT(TypeParamT(n), List[BaseType](), noops)) |
    identWithPath.map({
      case (path, name) => ExtAppT(path, name, List[BaseType](), noops)
    }) | (
      string("bool") *> ok(BoolT(noops)) |
      string("byte") *> ok(ByteT(noops)) |
      string("int") *> ok(IntT(noops)) |
      string("long") *> ok(LongT(noops)) |
      string("float") *> ok(FloatT(noops)) |
      string("string") *> ok(StringT(noops))).map(_.base[String])

  lazy val identWithPath: Parser[(List[String], String)] = (for {
    p <- uident <* char('.')
    n <- lident
  } yield (List(p), n)) |
  (for {
    p <- uident <* char('.')
    p1 <- identWithPath
    (p2, n) = p1
  } yield (p :: p2, n))

  lazy val tuple = "tuple" |:
    typeExprSimple.sepBy1(string("*")).map(x => CoreT(TupleT(x.list, noops))).parens

  def recordType(name: String, args: List[String]): Parser[Type] = "record type" |:
    braces(fieldList).map(fs =>
      RecordT(RecordDataType(name, fs.map(_.map(a => toScope(args)(a)))), noops))

  lazy val fieldList: Parser[List[Field[BaseType]]] = "field list" |: {
    (field <* string(";") |@| fieldList)(_ :: _) |
    (field <* string(";")).map(List(_)) |
    field.map(List(_))
  }

  lazy val field = "field" |: {
    (lident <* string(":") |@| typeExprSimple)(Field(_, false, _)) |
    (string("mutable") *> lident <* string(":") |@| typeExprSimple)(
      Field(_, true, _))
  }

  lazy val recordApp: Parser[MessageDef] =
    (lident <* string("<") |@|
      (typeExprSimple.sepBy1(string(",")) <* string(">")))((x, y) => AppM(x, y.list, noops)) |
    lident.map(AppM(_, Nil, noops))

  lazy val record = "record" |:
    braces(fieldList).map(RecordM(_))

  lazy val recordOrApp: Parser[MessageDef] = record | recordApp

  lazy val msgExpr: Parser[MessageDef] =
    recordOrApp | (uident flatMap complexMsgExpr)

  def complexMsgExpr(n: String): Parser[MessageDef] =
    (string(".") *> uident.sepBy(string(".")) |@| lident) {
      (p, name) => MessageAlias[String](n :: p, name)
    } |
    recordOrApp.map(x => SumM[String](List((n, x)))) |
    (for {
      x <- recordOrApp <* string("|")
      l <- (uident ~ recordOrApp).sepBy1(string("|"))
    } yield SumM[String]((n, x) :: l.list))

  lazy val lident = ident(lower) named "identifier starting with lowercase"
  lazy val uident = ident(upper) named "identifier starting with uppercase"

  def ident(p: Parser[Char]): Parser[String] =
    (p |@| takeWhile(isCont))(_ +: _).map(_.mkString)

  def isCont(c: Char) = Character.isLetterOrDigit(c) || c == '_' || c == '-'

  def takeWhile(p: Char => Boolean): Parser[List[Char]] = satisfy(p).many

}
