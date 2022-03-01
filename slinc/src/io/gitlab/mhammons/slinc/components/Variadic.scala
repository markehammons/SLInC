package io.gitlab.mhammons.slinc.components

import ffi.{ForeignSymbol}
import scala.quoted.*
import scala.util.chaining.*
import scala.util.NotGiven
import scala.annotation.implicitNotFound
import io.gitlab.mhammons.slinc.CLibrary

trait VariadicMechanisms:
   trait VariadicCall
   protected def variadicHandlerC[R](
       address: Expr[ForeignSymbol],
       cache: Expr[LRU],
       params: List[Expr[Any]],
       args: Expr[Seq[Any]]
   )(using Quotes, Type[R]) =
      import quotes.reflect.*
      val paramTypes = params.map { case '{ $v: t } =>
         TypeRepr.of[t].widen.asType
      }
      val (vParams, vTypes) = args match
         case Varargs(exprs) =>
            exprs
               .map(_.widen)
               .map { case '{ $v: a } =>
                  v -> Type.of[a]
               }
               .toList
               .unzip

      val mhLambda: Expr[Any] = MethodHandleMacros
         .wrappedMH(
           address,
           (paramTypes ++ vTypes).map(_ => None),
           paramTypes,
           Type.of[R],
           vTypes
         )
         .fold(e => report.errorAndAbort(e.mkString("\n")), identity)

      mhLambda match {
         case '{ $l: t } =>
            val lmb = '{
               $cache
                  .get(
                    ${
                       Expr(
                         vTypes.map { case '[a] => Type.show[a] }.mkString(";")
                       )
                    },
                    $l
                  )
            }.asTerm

            Apply(
              Select(
                lmb,
                TypeRepr.of[t].typeSymbol.declaredMethod("apply").head
              ),
              (params ++ vParams).map(_.asTerm)
            ).asExprOf[R]
      }
   end variadicHandlerC
