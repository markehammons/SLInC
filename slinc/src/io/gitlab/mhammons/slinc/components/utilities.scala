package io.gitlab.mhammons.slinc.components

import scala.quoted.*

import ffi.{Allocator, Address, Segment, ForeignSymbol, Lookup, Scope}
import scala.jdk.OptionConverters.*
import scala.compiletime.ops.boolean.!

val clookup: String => ForeignSymbol =
   (s: String) =>
      Lookup.systemLookup
         .lookup(s)
         .orElse(Lookup.loaderLookup.lookup(s))
         .getOrElse(throw new Exception(s"Couldn't find $s anywhere"))

extension (expr: Expr.type)
   def summonOrError[A: Type](using Quotes): Expr[A] = summonOrError[A]("")
   def summonOrError[A: Type](location: String)(using Quotes): Expr[A] =
      import quotes.reflect.{report}
      Expr
         .summon[A]
         .getOrElse(
           report.errorAndAbort(
             s"Could not summon ${Type.show[A]} in macro $location"
           )
         )

type Allocatee[A] = Allocator ?=> A

val segAlloc: Allocatee[Allocator] = summon[Allocator]

def allocate[A]: Allocatee[Informee[A, Segment]] = allocate(1)
def allocate[A](num: Long): Allocatee[Informee[A, Segment]] =
   segAlloc.allocateSegment(layoutOf[A].byteSize * num)

type Scopee[A] = Scope ?=> A

val currentScope: Scopee[Scope] = summon[Scope]

extension [A](t: Type[A])(using Quotes)
   def widen =
      given Type[A] = t
      import quotes.reflect.*
      TypeRepr.of[A].widen.asType

extension [A](t: Expr[?])(using Quotes)
   def widen: Expr[?] =
      import quotes.reflect.*
      t match
         case '{ $a: a } =>
            TypeRepr.of[a].widen.asType match
               case '[b] => '{ ${ a.asExprOf[b] }: b }

def findClass(using q: Quotes)(symbol: q.reflect.Symbol): q.reflect.Symbol =
   if symbol.isClassDef then symbol else findClass(symbol.owner)
def findMethod(using q: Quotes)(symbol: q.reflect.Symbol): q.reflect.Symbol =
   if symbol.isDefDef then symbol else findMethod(symbol.owner)
