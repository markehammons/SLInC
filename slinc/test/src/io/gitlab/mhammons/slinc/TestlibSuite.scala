package io.gitlab.mhammons.slinc

class TestlibSuite extends munit.FunSuite:
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
         Testlib.slinc_test_getstaticarr().rescope.toArray(3)
      }
      assertEquals(result(0), 1)
      assertEquals(result(1), 2)
      assertEquals(result(2), 3)
   }

   test("pass in as static arr") {
      scope {
         Testlib.slinc_test_passstaticarr(Array(1, 2, 3).serialize)
      }
   }

   test("should be able to pass in two structs") {
      val res = Testlib.slinc_two_structs(
        Testlib.a_t(1, 3),
        Testlib.a_t(4, 2)
      )

      assertEquals(res, 4)
   }
