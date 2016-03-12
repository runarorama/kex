package kex
package compiler
package codegen

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

class Compile(source: String) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro Compiler.impl
}

import scalaz.Leibniz._

object Make {
  def generateCode(gen: GenCode)(
    decls: List[Declaration]): gen.AST = {

      val bindings = GenCode.collectBindings(decls)

      gen.generateCode { for {
        decl <- decls
        cont <- gen.generateContainer(bindings, decl)
      } yield decl match {
        case TypeDecl(name, params, expr, opts) =>
          gen.typeDeclGenerator(bindings, name, params, expr, opts, cont)
        case MessageDecl(name, expr, opts) =>
          gen.msgDeclGenerator(bindings, name, expr, opts, cont)
      }
    }
  }
}

object Compiler extends MacrosCompatibility {

  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    import Flag._

    // Parse the source code
    val decls: List[Declaration] =
      Parser(c.prefix.tree match {
        case q"new $name($source)" =>
          c.eval(c.Expr[String](resetLocalAttrs(c)(source)))
      }).either.fold(e => c.abort(c.enclosingPosition, e), a => a)

    val gen = new GenScala.Generator(c)

    // Generate the scala code
    val body: gen.AST = Make.generateCode(gen)(decls)

    // Generate the target object
    val result = annottees.map(_.tree).toList match {
      case q"object $name extends ..$parents { ..$body }" :: Nil =>
        q"""
        object $name extends ..$parents {
          $body
        }
        """
      case _ => c.abort(c.enclosingPosition,
                        "ExtProt compiler must annotate an object declaration.")
    }

    c.Expr[Any](result)
  }
}
