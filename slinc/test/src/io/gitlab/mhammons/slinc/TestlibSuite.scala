package io.gitlab.mhammons.slinc

class TestlibSuite extends munit.FunSuite:
   import Testlib.*

   test("modify") {
      val a = Testlib.a_t(3, 4)
      val b = Testlib.b_t(5, a)

      assertEquals(
        Testlib.slinc_test_modify(b),
        b.copy(d = a.copy(a = 9))
      )
   }

   test("addone") {
      val c = Testlib.c_t(StaticArray[Int, 3], StaticArray[Float, 3])

      val result = Testlib.slinc_test_addone(c)

      c.a.underlying
         .zip(result.a.underlying)
         .foreach((o, r) => assertEquals(r, o + 1))
      c.b.underlying
         .zip(result.b.underlying)
         .foreach((o, r) => assertEquals(r, o + 1))
   }

   test("get static arr") {

      val result = scope {
         Testlib.slinc_test_getstaticarr().mkArray(3)
      }
      assertEquals(result(0), 1)
      assertEquals(result(1), 2)
      assertEquals(result(2), 3)
   }

   test("pass in as static arr") {
      scope {
         Testlib.slinc_test_passstaticarr(Array(1, 2, 3).encode)
      }
   }

   test("should be able to pass in two structs") {
      val res = Testlib.slinc_two_structs(
        Testlib.a_t(1, 3),
        Testlib.a_t(4, 2)
      )

      assertEquals(res, 4)
   }

   test("basic upcalls should be possible") {
      val res = scope {
         val fnPtr = (() => 3).encode
         Testlib.slinc_upcall(fnPtr)
      }

      assertEquals(res, 3)
   }

   test("struct returning upcalls should work") {
      val res = scope {
         val fnPtr = (() => Testlib.a_t(1, 3)).encode
         Testlib.slinc_upcall_a_t(fnPtr)
      }

      assertEquals(res, 4)
   }

   test("can call simple function pointers that come from C") {
      val res = Testlib.slinc_fptr_ret().deref

      assertEquals(res(), Testlib.a_t(3, 2))
   }

   test("can call slightly more complex function pointers that come from C") {
      val res = Testlib.slinc_fptr_ret2().deref

      assertEquals(res(1, 2), 3)
   }

   test("char works properly") {
      assertEquals(
        char_test('a'.asAsciiOrFail),
        'A'.asAsciiOrFail
      )
   }

   test("string works properly") {
      scope {
         assertEquals(string_test("hello world".encode).toChar, 'e')
      }
   }

   test("byte works properly") {
      assertEquals(byte_test(1), 2.toByte)
   }

   test("short works properly") {
      assertEquals(short_test(1), 3.toShort)
   }

   test("int works properly") {
      assertEquals(int_test(1), 4)
   }

   test("long works properly") {
      assertEquals(long_test(1), 5L)
   }

   test("boolean works properly") {
      assertEquals(bool_test(true), false)
      assertEquals(bool_test(false), false)
   }

   test("float works properly") {
      assertEquals(float_test(3f), 7f)
   }

   test("double works properly") {
      assertEquals(double_test(3d), 6d)
   }

   test("string passing via temporary allocator isn't safe".fail) {
      scope {
         bad_method("hello world".encode)
         assertEquals(ibreak("goodbye world".encode), "goodbye world")
      }
   }

   test("can serialize and deserialize int") {
      scope {
         assertEquals(4.encode.deref, 4)
      }
   }

   test("can serialize and deserialize boolean") {
      scope {
         assertEquals(true.encode.deref, true)
      }
   }

   test("can serialize and deserialize float") {
      scope {
         assertEquals(5.0f.encode.deref, 5.0f)
      }
   }

   test("can serialize and deserialize char") {
      scope {
         assertEquals('a'.asAsciiOrFail.encode.deref, 'a'.asAsciiOrFail)
      }
   }

   test("can't serialize and deserialize ʯ".fail) {
      scope {
         assertEquals('ʯ'.asAsciiOrFail.encode.deref, 'ʯ'.asAsciiOrFail)
      }
   }

   test("variadic function bindings") {
      assertEquals(sum(5).apply(34, 2, 3, 4, 5), 34 + 2 + 3 + 4 + 5)
   }
