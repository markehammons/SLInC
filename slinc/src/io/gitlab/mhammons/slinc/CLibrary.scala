package io.gitlab.mhammons.slinc

import java.nio.file.Paths
import components.ffi.Lookup
import components.Cache
import scala.quoted.*
import scala.compiletime.{erasedValue, constValue}
import scala.annotation.tailrec
class CLibrary[A](
    val cache: Cache,
    val lookup: Lookup
)
object CLibrary:
   inline def derived[A]: CLibrary[A] =
      val loader = inline erasedValue[A] match
         case _: LibraryLocation =>
            Lookup.loaderLookup
         case _ =>
            Lookup.systemLookup

      type prefix[A] = A match
         case WithPrefix[v] => v
         case _             => ""

      type rawCasing[A] = A match
         case RawNaming => true
         case _         => false

      CLibrary(Cache[A, prefix[A], rawCasing[A]](loader), loader)
