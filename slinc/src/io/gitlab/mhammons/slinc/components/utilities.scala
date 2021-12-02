package io.gitlab.mhammons.slinc.components

import scala.quoted.*

import jdk.incubator.foreign.{CLinker, SymbolLookup, MemoryAddress}
import scala.jdk.OptionConverters.*
import scala.compiletime.ops.boolean.!

val clookup: String => MemoryAddress =
   (s: String) =>
      CLinker.systemLookup
         .lookup(s)
         .toScala
         .orElse(SymbolLookup.loaderLookup.lookup(s).toScala)
         .getOrElse(throw new Exception(s"Couldn't find $s anywhere"))

def missingSegmentAllocator(using Quotes) =
   import quotes.reflect.report
   report.errorAndAbort(
     "A segment allocator is needed. If you're defining a bind, try making sure there is a `(using SegmentAllocator...` clause in the parameters."
   )

def missingBoundaryCrossing[A: Type](using Quotes) =
   import quotes.reflect.report
   report.errorAndAbort(
     s"A boundary crossing is missing for type ${Type.show[A]}"
   )

def missingLayout[A: Type](using Quotes) =
   import quotes.reflect.report
   report.errorAndAbort(
     s"No layout for type ${Type.show[A]} was found. Please define one if you need this type."
   )

def missingTemplate[A: Type](using Quotes) =
   import quotes.reflect.report
   report.errorAndAbort(
     s"No template for type ${Type.show[A]} was found. Please define one if you need this type"
   )
