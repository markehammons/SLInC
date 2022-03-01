import mill._, scalalib._
import scala.collection.immutable.LazyList
import scala.util.chaining._

trait VariadicGen extends ScalaModule {

   def callClassGen(arity: Int) = {
      val typeBlock = s"[${typeListForArity(arity)}]"
      val applyArgument = ("address" +: paramNames.take(arity) :+ "args")
         .map(v => s"'$v")
         .mkString(",")
      s"""| case class VariadicCall$arity$typeBlock${constructor(arity)}{
          |    inline def apply(inline args: Any*): $returnType = $${
          |       variadicCall${arity}Impl$typeBlock($applyArgument)
          |    }
          | }""".stripMargin
   }

   def cachedCallClassGen(arity: Int) = {
      val typeBlock = s"[${typeListForArity(arity)}]"
      val applyArgument =
         ("address" +: "cache" +: paramNames.take(arity) :+ "args")
            .map(v => s"'$v")
            .mkString(",")
      s"""| case class CachedVariadicCall$arity$typeBlock${cachedConstructor(arity)} extends VariadicCall{
          |    inline def apply(inline args: Any*): $returnType = $${
          |       variadicCallC${arity}Impl$typeBlock($applyArgument)
          |    }
          | }""".stripMargin
   }

   def macroImplGen(arity: Int) = {
      val typeBlock = s"[${typeListForArity(arity)}]"
      val args = (("address" -> "ForeignSymbol") +: paramNames
         .zip(typeNames)
         .take(arity)
         .toList :+ ("args" -> "Seq[Any]"))
         .map { case (p, t) =>
            s"$p:Expr[$t]"
         }
         .toList
         .mkString(",")
      val using = ("Quotes" +: (typeNames.take(arity) :+ returnType).map(t =>
         s"Type[$t]"
      )).mkString(",")
      val argList = paramNames
         .take(arity)
         .map { p =>
            s"$p"
         }
         .mkString(",")
      s"""| private def variadicCall${arity}Impl$typeBlock(
          |    $args
          | )(using
          |    $using
          | ): Expr[$returnType] = {
          |    variadicHandler(address, List($argList), args)
          | }""".stripMargin
   }

   def cachedMacroImplGen(arity: Int) = {
      val typeBlock = s"[${typeListForArity(arity)}]"
      val args =
         (("address" -> "ForeignSymbol") +: ("cache" -> "LRU") +: paramNames
            .zip(typeNames)
            .take(arity)
            .toList :+ ("args" -> "Seq[Any]"))
            .map { case (p, t) =>
               s"$p:Expr[$t]"
            }
            .toList
            .mkString(",")
      val using = ("Quotes" +: (typeNames.take(arity) :+ returnType).map(t =>
         s"Type[$t]"
      )).mkString(",")
      val argList = paramNames
         .take(arity)
         .map { p =>
            s"$p"
         }
         .mkString(",")
      s"""| private def variadicCallC${arity}Impl$typeBlock(
          |    $args
          | )(using
          |    $using
          | ): Expr[$returnType] = {
          |    variadicHandlerC(address, cache, List($argList), args)
          | }""".stripMargin
   }

   def variadicCallsGen(limit: Int) =
      s"""|package io.gitlab.mhammons.slinc.components
          |
          |import ffi.ForeignSymbol
          |import scala.quoted.*
                                       |object VariadicCalls extends VariadicMechanisms {
                                       |${(for (arity <- 0 until limit)
         yield cachedCallClassGen(arity)).mkString("\n")}
                                       |${(for (arity <- 0 until limit)
         yield cachedMacroImplGen(arity)).mkString("\n")}
                                       |}""".stripMargin

   val returnType = "AA"
   val typeNames = LazyList.iterate('A'.toInt, 24)(_ + 1).map(_.toChar.toString)
   val paramNames =
      LazyList.iterate('a'.toInt, 24)(_ + 1).map(_.toChar.toString)
   def cachedConstructor(arity: Int) =
      (("address" -> "ForeignSymbol") +: ("cache" -> "LRU") +: paramNames
         .zip(typeNames)
         .take(arity))
         .map { case (p, t) => s"$p:$t" }
         .mkString(",")
         .pipe(ps => s"($ps)")
   def constructor(arity: Int) = (("address" -> "ForeignSymbol") +: paramNames
      .zip(typeNames)
      .take(arity))
      .map { case (p, t) => s"$p:$t" }
      .mkString(",")
      .pipe(ps => s"($ps)")
   def typeListForArity(arity: Int) =
      (typeNames.take(arity) :+ returnType).mkString(",")

   def generateVariadicCalls = T {
      val dest = T.dest / s"VariadicCalls.scala"
      os.write(dest, variadicCallsGen(22))
      PathRef(dest)
   }

   override def generatedSources = T {
      super.generatedSources() :+ generateVariadicCalls()
   }

}
