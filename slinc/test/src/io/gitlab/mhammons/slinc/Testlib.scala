package io.gitlab.mhammons.slinc

import jdk.incubator.foreign.SegmentAllocator

object BuildInfo {
   val libtest = ""
}
object Testlib extends Library(Location.Absolute(BuildInfo.libtest)):
   case class a_t(a: Int, b: Int) derives Struct
   case class b_t(c: Int, d: a_t) derives Struct

   case class d_t(p: Ptr[a_t]) derives Struct

   scope {
      val a = a_t(3, 2).serialize

      val x: Ptr[d_t] = d_t(a).serialize
      a.partial.b
      val r = x.partial.p

      println(x.partial.p.deref.b.deref)
   }

   case class c_t(a: StaticArray[Int, 3], b: StaticArray[Float, 3])
       derives Struct

   def slinc_test_modify(b_t: b_t): b_t = bind
   def slinc_test_addone(c_t: c_t): c_t = bind
   def slinc_test_getstaticarr(): Ptr[Int] = bind
   def slinc_test_passstaticarr(res: Ptr[Int]): Unit = bind
   def slinc_two_structs(a: a_t, b: a_t): Int = bind
   def slinc_upcall(zptr: Ptr[() => Int]): Int = bind
   def slinc_upcall_a_t(zptr: Ptr[() => a_t]): Int = bind
   def slinc_fptr_ret(): Ptr[() => a_t] = bind
   def slinc_fptr_ret2(): Ptr[(Int, Int) => Int] = bind
