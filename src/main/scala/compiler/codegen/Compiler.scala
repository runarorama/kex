package kex
package compiler
package codegen

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

class Compile(source: String) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro Compiler.impl
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

    // Generate the scala code
    val body: Tree = ??? // codegen.GenCode.generateScala(c)(decls)

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
