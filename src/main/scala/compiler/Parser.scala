package extprot
package compiler

import scalaz._
import Scalaz._
import bound._

object Parser {
  import ProtocolTypes._

  def apply(s: String) = runParser(declarations, s) match {
    case Left(e) => \/.left(e.pretty.toString)
    case Right((_, r)) => \/.right(r)
  }

  import scalaparsers.Document._
  import scalaparsers.Diagnostic._

  val P = new scalaparsers.Parsing[Unit] {}
  import P._

  implicit class Scope[A](p: Parser[A]) {
    def |:(s: String): Parser[A] = p scope s
  }

  // Skip lines, comments, or horizontal white space
  lazy val skipLWS: Parser[Unit] = (newline | whitespace).skipMany

  // Skip comments or horizontal whitespace
  lazy val skipHWS: Parser[Unit] = whitespace.skipMany

  lazy val declarations: Parser[List[Declaration]] = declaration.many

  lazy val messageDecl: Parser[Declaration] = "message declaration" |: {
    for {
      name <- token("message") >> lident << token("=")
      e <- msgExpr
      opts <- typeOptions
    } yield MessageDecl(name, e, opts)
  }

  lazy val typeDecl: Parser[Declaration] = "type declaration" |: {
    for {
      name <- token("type") >> lident
      par <- "'" >> lident.many
      e <- token("=") >> typeExpr(name, par)
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
    skipLWS >> (messageDecl | typeDecl)

  lazy val typeOptions: Parser[TypeOptions] = "type options" |: {
    (token("options") >> typeOptionValues.many.map(_.toMap)) | unit(noops)
  }

  lazy val newline: Parser[Unit] =
    satisfy(c => c == '\r' || c == '\n').skip scope "newline"

  lazy val whitespace: Parser[Unit] =
    satisfy(c => c.isWhitespace && c != '\r' && c != '\n').skip scope "whitespace"

  lazy val typeOptionValues =
    (stringLiteral << token("=")).map2(stringLiteral)((_, _))

  def toScope(args: List[String])(t: BaseType): BaseScope[String] =
    abstrakt[BaseTypeExpr, String, String](t)(x => args find (a => a == x))

  def typeExpr(name: String, args: List[String]): Parser[Type] = (s"top-level type $name" |:(
    constDeclarations.sepBy1(token("|")).map(
      cs => SumT(SumDataType(name, cs.map(_.map(toScope(args)))), noops)) |
    recordType(name, args)
  )) | (s"simple type $name" |: typeExprSimple.map(t => BaseT(toScope(args)(t))))

  lazy val constDeclarations: Parser[DataConstructor[BaseType]] =
    "data constructor declaration" |: {
      uident.map2(constParams)(NonConstant[BaseType](_,_)) |
      uident.map(Constant[BaseType](_))
    }

  lazy val constParams: Parser[List[BaseType]] =
    typeExprSimple.some

  def bracket[A](p: => Parser[A]) = leftBracket >> (p << right)
  def envelope[A](p: => Parser[A]) = leftEnvelope >> (p << right)
  def brace[A](p: => Parser[A]) = leftBrace >> (p << right)

  lazy val typeExprSimple: Parser[BaseType] =
    (bracket(typeExprSimple).map(x => CoreT(ListT(x, noops))) |
     envelope(typeExprSimple).map(x => CoreT(ArrayT(x, noops))) |
     tuple |
     (for {
       n <- lident << token("<")
       targs <- typeExprSimple.sepBy1(token(",")) << token(">")
     } yield AppT(TypeParamT(n), targs, noops)).attempt |
     (token("'") >> lident.map(TypeParamT(_))) |
     lident.map(n => AppT(TypeParamT(n), Nil, noops)) |
     identWithPath.map({
       case (path, name) => ExtAppT(path, name, Nil, noops)
     })) |
    (token("bool") >> unit(BoolT(noops)) |
    token("byte") >> unit(ByteT(noops)) |
    token("int") >> unit(IntT(noops)) |
    token("long") >> unit(LongT(noops)) |
    token("float") >> unit(FloatT(noops)) |
    token("string") >> unit(StringT(noops))).map(_.base)

  lazy val identWithPath: Parser[(List[String], String)] = (for {
    p <- uident << "."
    n <- lident
  } yield (List(p), n)) |
  (for {
    p <- uident << "."
    p1 <- identWithPath
    (p2, n) = p1
  } yield (p :: p2, n))

  lazy val tuple = "tuple" |:
    paren(typeExprSimple.sepBy1(token("*"))).map(x => CoreT(TupleT(x, noops)))

  def recordType(name: String, args: List[String]): Parser[Type] = "record type" |:
    brace(fieldList).map(fs =>
      RecordT(RecordDataType(name, fs.map(_.map(a => toScope(args)(a)))), noops))

  lazy val fieldList: Parser[List[Field[BaseType]]] = "field list" |: {
    (field << token(";")).map2(fieldList)(_ :: _).attempt |
    (field << token(";")).map(List(_)).attempt |
    field.map(List(_))
  }

  lazy val field = "field" |: {
    (lident << token(":")).map2(typeExprSimple)(Field(_, false, _)) |
    (token("mutable") >> lident << token(":")).map2(typeExprSimple)(
      Field(_, true, _))
  }

  lazy val recordApp: Parser[MessageDef] =
    (lident << token("<")).map2(
      typeExprSimple.sepBy1(token(",")) << token(">"))(AppM(_, _, noops)) |
    lident.map(AppM(_, Nil, noops))

  lazy val record = "record" |:
    brace(fieldList).map(RecordM(_))

  lazy val recordOrApp: Parser[MessageDef] = record | recordApp

  lazy val msgExpr: Parser[MessageDef] =
    recordOrApp | (uident flatMap complexMsgExpr)

  def complexMsgExpr(n: String): Parser[MessageDef] =
    (token(".") >> uident.sepBy(token("."))).map2(lident) {
      (p, name) => MessageAlias[String](n :: p, name)
    } |
    recordOrApp.map(x => SumM[String](List((n, x)))) |
    (for {
      x <- recordOrApp << token("|")
      l <- (uident ++ recordOrApp).sepBy1(token("|"))
    } yield SumM[String]((n, x) :: l))

  lazy val lident = ident(lower) scope "identifier starting with lowercase"
  lazy val uident = ident(upper) scope "identifier starting with uppercase"

  def ident(p: Parser[Char]): Parser[String] =
    token(p.map2(takeWhile(isCont))(_ +: _).map(_.mkString))

  def isCont(c: Char) = Character.isLetterOrDigit(c) || c == '_' || c == '-'

  def takeWhile(p: Char => Boolean): Parser[List[Char]] = satisfy(p).many

  import scalaparsers.ParseState
  import scalaparsers.Supply
  import scalaparsers.Pos

  def apply(input: String, fileName: String) =
    runParser(declarations, input, fileName)

  def runParser[A](p: Parser[A], input: String, fileName: String = "") =
    p.run(ParseState(
      loc = Pos.start(fileName, input),
      input = input,
      s = (),
      layoutStack = List()), Supply.create)
}
