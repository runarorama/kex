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

  lazy val declarations: Parser[List[Declaration]] = declaration.many

  lazy val messageDecl: Parser[Declaration] = "message declaration" |: {
    for {
      name <- "message" >> lident << "="
      e <- msgExpr
      opts <- typeOptions
    } yield MessageDecl(name, e, opts)
  }

  lazy val typeDecl: Parser[Declaration] = "type declaration" |: {
    for {
      name <- "type" >> lident
      par <- ("'" >> lident.map(TypeParam.fromString(_))).many
      e <- "=" >> typeExpr
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
    messageDecl | typeDecl

  lazy val typeOptions: Parser[TypeOptions] = "type options" |: {
    ("options" >> typeOptionValues.many.map(_.toMap)) | unit(noops)
  }

  lazy val typeOptionValues =
    (stringLiteral << "=").map2(stringLiteral)((_, _))

  lazy val typeExpr: Parser[TypeExpr] = ("top-level type" |:(
    constDeclarations.sepBy1("|").map(cs => SumT(SumDataType("bogus", cs), noops)) |
    recordType
  )) | ("simple type" |: typeExprSimple).map(BaseT)

  lazy val constDeclarations: Parser[DataConstructor[BaseTypeExpr]] =
    "data constructor declaration" |: {
      uident.map2(constParams)(NonConstant[BaseTypeExpr](_,_)) |
      uident.map(Constant[BaseTypeExpr](_))
    }

  lazy val constParams: Parser[List[BaseTypeExpr]] =
    typeExprSimple.some

  lazy val typeExprSimple: Parser[BaseTypeExpr] =
    (bracket(typeExprSimple).map(x => CoreT(ListT(x, noops))) |
     envelope(typeExprSimple).map(x => CoreT(ArrayT(x, noops))) |
     tuple |
     (for {
       n <- lident << "<"
       targs <- typeExprSimple.sepBy1(",") << ">"
     } yield AppT(n, targs, noops)) |
     ("'" >> lident.map(n => TypeParamT(TypeParam.fromString(n)))) |
     lident.map(AppT(_, Nil, noops)) |
     identWithPath.map({
       case (path, name) => ExtAppT(path, name, Nil, noops)
     })) |
    ("bool" >> unit(BoolT(noops)) |
    "byte" >> unit(ByteT(noops)) |
    "int" >> unit(IntT(noops)) |
    "long" >> unit(LongT(noops)) |
    "float" >> unit(FloatT(noops)) |
    "string" >> unit(StringT(noops))).map(_.base)

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
    paren(typeExprSimple.sepBy1("*")).map(x => CoreT(TupleT(x, noops)))

  lazy val recordType: Parser[TypeExpr] = "record type" |:
    brace(fieldList).map(fs => RecordT(RecordDataType("bogus", fs), noops))

  lazy val fieldList: Parser[List[Field[BaseTypeExpr]]] = "field list" |: {
    (field << ";").map2(fieldList)(_ :: _) |
    (field << ";").map(List(_)) |
    field.map(List(_))
  }

  lazy val field = "field" |: {
    (lident << ":").map2(typeExprSimple)(Field(_, false, _)) |
    ("mutable" >> lident << ":").map2(typeExprSimple)(Field(_, true, _))
  }

  lazy val recordApp =
    (lident << "<").map2(typeExprSimple.sepBy1(",") << ">")(AppM(_, _, noops)) |
    lident.map(AppM(_, Nil, noops))

  lazy val record = "record" |:
    brace(fieldList).map(BaseM(_))

  lazy val recordOrApp: Parser[MessageExpr] = record | recordApp

  lazy val msgExpr: Parser[MessageExpr] =
    recordOrApp | (uident flatMap complexMsgExpr)

  def complexMsgExpr(n: String): Parser[MessageExpr] =
    ("." >> uident.sepBy(".")).map2(lident) {
      (p, name) => MessageAlias(n :: p, name)
    } |
    recordOrApp.map(x => SumM(List((n, x)))) |
    (for {
      x <- recordOrApp << "|"
      l <- (uident ++ recordOrApp).sepBy1("|")
    } yield SumM((n, x) :: l))

  lazy val lident = ident(lower) scope "identifier starting with lowercase"
  lazy val uident = ident(upper) scope "identifier starting with uppercase"

  def ident(p: Parser[Char]): Parser[String] =
    p.map2(takeWhile(isCont))(_ +: _).map(_.mkString)

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
