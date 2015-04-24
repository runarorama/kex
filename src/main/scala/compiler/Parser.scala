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

  def apply(s: String): ParseResult[List[Declaration]] = parseOnly(declarations, s)

  implicit class Scope[A](p: Parser[A]) {
    def |:(s: String): Parser[A] = p named s
  }

  // Skip lines, comments, or horizontal white space
  lazy val skipLWS: Parser[Unit] = (newline | whitespace | comment).skipMany

  // Skip comments or horizontal whitespace
  lazy val skipHWS: Parser[Unit] = (whitespace | comment).skipMany

  lazy val comment: Parser[Unit] = string("(*") ~> manyUntil(anyChar, t("*)")).void

  lazy val declarations: Parser[List[Declaration]] = skipLWS *> declaration.many

  lazy val messageDecl: Parser[Declaration] = "message declaration" |: {
    for {
      name <- t("message") *> t(lident) <* t("=")
      e <- msgExpr
      opts <- typeOptions
    } yield MessageDecl(name, e, opts)
  }

  def t(s: String): Parser[String] = string(s) <* skipLWS
  def t[A](p: Parser[A]): Parser[A] = p <* skipLWS

  lazy val typeDecl: Parser[Declaration] = "type declaration" |: {
    for {
      name <- t("type") *> t(lident)
      par <- t(string("'") *> lident).many
      e <- t("=") *> typeExpr(name, par)
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
    t(messageDecl | typeDecl)

  lazy val typeOptions: Parser[TypeOptions] = "type options" |: {
    (t("options") *> typeOptionValues.many.map(_.toMap)) | ok(noops)
  }

  lazy val newline: Parser[Unit] =
    skip(c => c == '\r' || c == '\n') named "newline"

  lazy val whitespace: Parser[Unit] =
    skip(c => c.isWhitespace && c != '\r' && c != '\n') named "whitespace"

  lazy val typeOptionValues =
    (t(stringLiteral) <* t("=")) ~ t(stringLiteral)

  def toScope(args: List[String])(t: BaseType): BaseScope[String] =
    abstrakt[BaseTypeExpr, String, String](t)(x => args find (a => a == x))

  def typeExpr(name: String, args: List[String]): Parser[Type] = (s"top-level type $name" |:(
    constDeclaration.sepBy1(t("|")).map(
      cs => SumT(SumDataType(name, cs.map(_.map(toScope(args))).list), noops)) |
    recordType(name, args)
  )) | (s"simple type $name" |: typeExprSimple.map(t => BaseT(toScope(args)(t))))

  lazy val constDeclaration: Parser[DataConstructor[BaseType]] =
    "data constructor declaration" |: {
      ^(t(uident), constParams)(NonConstant[BaseType](_,_)) |
      t(uident).map(Constant[BaseType](_))
    }

  lazy val constParams: Parser[NonEmptyList[BaseType]] =
    typeExprSimple.many1

  lazy val typeExprSimple: Parser[BaseType] =
    t(squareBrackets(typeExprSimple)).map(x => CoreT(ListT(x, noops))).named("list type") |
    t(envelopes(typeExprSimple)).map(x => CoreT(ArrayT(x, noops))).named("array type") |
    t(tuple).named("tuple type") |
    (for {
      n <- t(lident) <* t("<")
      targs <- typeExprSimple.sepBy1(t(",")) <* t(">")
    } yield AppT(TypeParamT(n), targs.list, noops)).named("type application") |
    (string("'") *> t(lident).map(TypeParamT(_))).named("type parameter") |
    t(lident).map(n => AppT(TypeParamT(n), List[BaseType](), noops)).named("unapplied type") |
    t(identWithPath).map({
      case (path, name) => ExtAppT(path, name, List[BaseType](), noops)
    }).named("external type application") | (
      t("bool") *> ok(BoolT(noops)) |
      t("byte") *> ok(ByteT(noops)) |
      t("int") *> ok(IntT(noops)) |
      t("long") *> ok(LongT(noops)) |
      t("float") *> ok(FloatT(noops)) |
      t("string") *> ok(StringT(noops))).named("base type").map(_.base[String])

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
    typeExprSimple.sepBy1(t("*")).map(x => CoreT(TupleT(x.list, noops))).parens

  def recordType(name: String, args: List[String]): Parser[Type] = "record type" |:
    braces(fieldList).map(fs =>
      RecordT(RecordDataType(name, fs.map(_.map(a => toScope(args)(a)))), noops))

  lazy val fieldList: Parser[List[Field[BaseType]]] = "field list" |: {
    ^(t(field) <* t(";"), fieldList)(_ :: _) |
     (t(field) <* t(";")).map(List(_)) |
      t(field).map(List(_))
  }

  lazy val field = "field" |: {
    ^(t(lident) <* t(":"), typeExprSimple)(Field(_, false, _)) |
    ^(t("mutable") *> t(lident) <* t(":"), typeExprSimple)(Field(_, true, _))
  }

  lazy val recordApp: Parser[MessageDef] = "record application" |: {
    ^(t(lident) <* t("<"), typeExprSimple.sepBy1(t(",")) <* t(">")) {
      (x, y) => AppM(x, y.list, noops)
    } | lident.map(AppM(_, List[BaseType](), noops))
  }

  lazy val record = "record" |:
    t(braces(fieldList)).map(RecordM(_))

  lazy val recordOrApp: Parser[MessageDef] = record | recordApp

  lazy val msgExpr: Parser[MessageDef] = "message expression" |:
     (recordOrApp | (t(uident) flatMap complexMsgExpr))

  def complexMsgExpr(n: String): Parser[MessageDef] = "complex message expression" |: {
    ("message alias" |: {
      ^(t(string(".") *> uident.sepBy(string("."))), t(lident)) {
        (p, name) => MessageAlias[String](n :: p, name)
      }
    }) |
    ("message sum type" |: (for {
      x <- recordOrApp <* t("|")
      l <- (t(uident) ~ recordOrApp).sepBy1(t("|"))
    } yield SumM[String]((n, x) :: l.list))) |
    recordOrApp.map(x => SumM[String](List((n, x))))
  }

  val keywords = List("type", "message", "mutable", "options",
                      "bool", "byte", "int", "long", "float", "string")

  def checkKeywords(s: String): Parser[String] =
    if (keywords contains s) err(s"Unexpected keyword $s") else ok(s)

  lazy val lident = ident(lower).flatMap(checkKeywords) named "identifier starting with lowercase"
  lazy val uident = ident(upper) named "identifier starting with uppercase"

  def ident(p: Parser[Char]): Parser[String] =
    ^(p, takeWhile(isCont))(_ +: _).map(_.mkString)

  def isCont(c: Char) = Character.isLetterOrDigit(c) || c == '_' || c == '-' || c == '''

  def takeWhile(p: Char => Boolean): Parser[List[Char]] = satisfy(p).many

}
