package io.gitlab.mhammons.slinc.components

import scala.quoted.*
import scala.util.chaining.*
import ffi.*
import cats.data.Validated

class Cache(storage: Array[Any]):
   def getCached[A](key: Int): A = storage(key).asInstanceOf[A]

object Cache:
   transparent inline def apply[A, S, RC](
       symbolLookup: Lookup
   ): Cache = ${
      cacheImpl[A, S, RC]('symbolLookup)
   }

   def isCachedSymbol(using q: Quotes)(
       s: q.reflect.Symbol
   ): Boolean =
      import quotes.reflect.*
      val blacklist = Set("writeReplace")

      def isNotNative(s: Symbol) = s.annotations.exists {
         case Apply(Select(New(tpt), _), _)
             if tpt.tpe =:= TypeRepr.of[NonNative] =>
            true
         case _ => false
      }

      !(blacklist.contains(s.name) || isNotNative(s))

   def isVariadic(using q: Quotes)(
       s: q.reflect.Symbol
   ): Boolean =
      import quotes.reflect.*

      s.tree match
         case DefDef(_, _, ret, _) =>
            ret.tpe.asType match
               case '[VariadicCalls.VariadicCall] => true
               case _                             => false

   def parseName(raw: Boolean, name: String): String = if raw then name
   else name.flatMap(c => if c.isUpper then s"_${c.toLower}" else c.toString)

   private def getAddress(using q: Quotes)(
       lookup: Expr[Lookup],
       prefix: String,
       raw: Boolean,
       s: q.reflect.Symbol
   ): Expr[ForeignSymbol] =
      val cleanedPrefix =
         prefix
            .filter(_ != '"')
            .pipe(str => if str.nonEmpty then s"${str}_" else str)
      val name =
         Expr(s"$cleanedPrefix${parseName(raw, s.name)}")
      '{
         val nm = ${ name }
         $lookup
            .lookup(nm).getOrElse(
               throw new Exception(s"Could not find $nm to bind to.")
            )
      }

   private def cacheImpl[A, S, RC](
       symbExpr: Expr[Lookup]
   )(using q: Quotes, t: Type[A], s: Type[S], rc: Type[RC]) =
      import quotes.reflect.*

      val raw = rc match
         case '[ true]  => true
         case '[ false] => false
      TypeRepr
         .of[A]
         .classSymbol
         .tap(_.map(_.name))
         .map(cs =>
            cs.declaredMethods
               .map(s =>
                  if isVariadic(s) then
                     Validated.valid(List('{
                        VariadicCache(${
                           getAddress(using q)(
                             symbExpr,
                             Type.show[S],
                             raw,
                             s
                           )
                        })
                     }))
                  else if isCachedSymbol(s) then
                     MethodHandleMacros
                        .wrappedMHFromDefDef(
                          s,
                          getAddress(symbExpr, Type.show[S], raw, s)
                        )
                        .bimap(
                          es =>
                             List(
                               s"Errors while generating binding for ${s.fullName}:\n\t- ${es.mkString("\n\t- ")}"
                             ),
                          List(_)
                        )
                  else Validated.valid(List('{ null }))
               )
               .reduce((a, b) => a.andThen(subA => b.map(subA ++ _)))
         )
         .map { s =>
            '{
               new Cache(Array(${
                  Varargs(
                    s.fold(
                      es => report.errorAndAbort(es.mkString("\n")),
                      identity
                    )
                  )
               }*))
            }
         }
         .getOrElse(
           report.errorAndAbort(
             s"Class not found for type ${Type.show[A]}... Are you sure you tried to derive CLibrary for a class or object?"
           )
         )
