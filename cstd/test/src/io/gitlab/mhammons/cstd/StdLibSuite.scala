package io.gitlab.mhammons.cstd

import io.gitlab.mhammons.slinc.*
import scala.util.Random

class StdLibSuite extends munit.FunSuite:
   import StdLib.*
   import CLocale.*

   test("abs") {
      assertEquals(
        abs(5),
        Math.abs(5)
      )
   }

   test("atof") {
      val r = scope {
         setlocale(LCNumeric, "C".encode)
         atof("5.3".encode)
      }

      assertEqualsDouble(r, 5.3, 0.001)
   }

   test("div") {
      val result = scope {
         div(5, 2)
      }
      assertEquals(result, DivT(2, 1))
   }

   test("getenv") {
      assertEquals(
        scope(getenv("PATH".encode)),
        System.getenv("PATH")
      )
   }

   test("labs") {
      assertEquals(
        labs(-5L),
        5L
      )
   }

   test("malloc") {
      val size = SizeT.fromByte(8)
      val ptr = malloc(size).castTo[Byte]
      !ptr = 'a'.toByte
      assertEquals(ptr.deref.toChar, 'a')
      free(ptr.castTo[Any])
   }

   test("qsort") {
      val base = Array.fill(30)(Random.nextInt.toLong)

      val sortedBase = scope {
         val copy = base.encode.castTo[Any]

         val fn =
            (a1: Ptr[Any], b: Ptr[Any]) =>
               if !a1.castTo[Long] > !b.castTo[Long] then 1
               else if !a1.castTo[Long] == !b.castTo[Long] then 0
               else -1
         val fnPtr = fn.encode
         qsort(
           copy.castTo[Any],
           SizeT.fromShortOrFail(base.size.toShort),
           sizeOf[Long],
           fnPtr
         )

         copy.castTo[Long].mkArray(base.size)
      }

      assertEquals(sortedBase.toSeq, base.toSeq.sorted)
   }

   test("strtod") {
      val (num, string) = scope {
         setlocale(LCNumeric, "C".encode)
         val str = "20.30300 This is a test".encode
         val strPtr = str.encode
         val number = strtod(str, strPtr)
         (number, strPtr.deref.mkString)
      }

      assertEqualsDouble(num, 20.303, 0.0001)
      assertEquals(string, " This is a test")
   }
