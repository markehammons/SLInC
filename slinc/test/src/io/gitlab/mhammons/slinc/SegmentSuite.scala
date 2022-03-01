package io.gitlab.mhammons.slinc


import components.ffi.{Allocator, Scope}

class SegmentSuite extends munit.FunSuite {
   given Allocator = Allocator.arenaAllocator(Scope.globalScope)
   // val i = int.allocate

   // test("can allocate int") {
   //    int.allocate
   // }

   // test("can apply and update int") {
   //    val i = int.allocate
   //    assertEquals(i(), 0)
   //    i() = 5
   //    assertEquals(i(), 5)
   // }

}
