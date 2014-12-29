package extprot
package compiler

import scalaz._
import scalaz.\/._

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
      par <- ("'" >> lident.map(TypeParam.fromString(_))).many
      e <- token("=") >> typeExpr
      opts <- typeOptions
    } yield {
      val ee = e match {
        case SumT(s, opts) => SumT(s copy (name = name), opts)
        case RecordT(r, opts) => RecordT(r copy (name = name), opts)
        case _ => e
      }
      TypeDecl(name, par, e, opts)
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

  lazy val typeExpr: Parser[TypeExpr] = ("top-level type" |:(
    constDeclarations.sepBy1(token("|")).map(
      cs => SumT(SumDataType("bogus", cs), noops)) |
    recordType
  )) | ("simple type" |: typeExprSimple).map(BaseT)

  lazy val constDeclarations: Parser[DataConstructor[BaseTypeExpr]] =
    "data constructor declaration" |: {
      uident.map2(constParams)(NonConstant[BaseTypeExpr](_,_)) |
      uident.map(Constant[BaseTypeExpr](_))
    }

  lazy val constParams: Parser[List[BaseTypeExpr]] =
    typeExprSimple.some

  def bracket[A](p: => Parser[A]) = leftBracket >> (p << right)
  def envelope[A](p: => Parser[A]) = leftEnvelope >> (p << right)
  def brace[A](p: => Parser[A]) = leftBrace >> (p << right)

  lazy val typeExprSimple: Parser[BaseTypeExpr] =
    (bracket(typeExprSimple).map(x => CoreT(ListT(x, noops))) |
     envelope(typeExprSimple).map(x => CoreT(ArrayT(x, noops))) |
     tuple |
     (for {
       n <- lident << token("<")
       targs <- typeExprSimple.sepBy1(token(",")) << token(">")
     } yield AppT(n, targs, noops)).attempt |
     (token("'") >> lident.map(n => TypeParamT(TypeParam.fromString(n)))) |
     lident.map(AppT(_, Nil, noops)) |
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

  lazy val recordType: Parser[TypeExpr] = "record type" |:
    brace(fieldList).map(fs => RecordT(RecordDataType("bogus", fs), noops))

  lazy val fieldList: Parser[List[Field[BaseTypeExpr]]] = "field list" |: {
    (field << token(";")).map2(fieldList)(_ :: _).attempt |
    (field << token(";")).map(List(_)).attempt |
    field.map(List(_))
  }

  lazy val field = "field" |: {
    (lident << token(":")).map2(typeExprSimple)(Field(_, false, _)) |
    (token("mutable") >> lident << token(":")).map2(typeExprSimple)(
      Field(_, true, _))
  }

  lazy val recordApp =
    (lident << token("<")).map2(
      typeExprSimple.sepBy1(token(",")) << token(">"))(AppM(_, _, noops)) |
    lident.map(AppM(_, Nil, noops))

  lazy val record = "record" |:
    brace(fieldList).map(RecordM(_))

  lazy val recordOrApp: Parser[MessageExpr] = record | recordApp

  lazy val msgExpr: Parser[MessageExpr] =
    recordOrApp | (uident flatMap complexMsgExpr)

  def complexMsgExpr(n: String): Parser[MessageExpr] =
    (token(".") >> uident.sepBy(token("."))).map2(lident) {
      (p, name) => MessageAlias(n :: p, name)
    } |
    recordOrApp.map(x => SumM(List((n, x)))) |
    (for {
      x <- recordOrApp << token("|")
      l <- (uident ++ recordOrApp).sepBy1(token("|"))
    } yield SumM((n, x) :: l))

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
