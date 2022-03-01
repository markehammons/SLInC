package io.gitlab.mhammons.slinc

import scala.annotation.tailrec
object Testlib extends AbsoluteLocation(BuildInfo.libtest) derives CLibrary:
   case class a_t(a: Int, b: Int) derives Struct
   case class b_t(c: Int, d: a_t) derives Struct

   case class d_t(p: Ptr[a_t]) derives Struct
   case class c_t(a: StaticArray[Int, 3], b: StaticArray[Float, 3])
       derives Struct

   def slinc_test_modify(b_t: b_t) = accessNative[b_t]
   def slinc_test_addone(c_t: c_t) = accessNative[c_t]
   def slinc_test_getstaticarr(): Ptr[Int] = accessNative[Ptr[Int]]
   def slinc_test_passstaticarr(res: Ptr[Int]): Unit = accessNative[Unit]
   def slinc_two_structs(a: a_t, b: a_t): Int = accessNative[Int]
   def slinc_upcall(zptr: Ptr[() => Int]): Int = accessNative[Int]
   def slinc_upcall_a_t(zptr: Ptr[() => a_t]): Int = accessNative[Int]
   def slinc_fptr_ret(): Ptr[() => a_t] = accessNative[Ptr[() => a_t]]
   def slinc_fptr_ret2(): Ptr[(Int, Int) => Int] =
      accessNative[Ptr[(Int, Int) => Int]]

   def byte_test(b: Byte): Byte = accessNative[Byte]
   def short_test(a: Short): Short = accessNative[Short]
   def int_test(a: Int): Int = accessNative[Int]
   def long_test(a: Long): Long = accessNative[Long]
   def char_test(a: AsciiChar): AsciiChar = accessNative[AsciiChar]
   def string_test(str: Ptr[Byte]): AsciiChar = accessNative[AsciiChar]
   def bool_test(a: Boolean): Boolean = accessNative[Boolean]
   def float_test(f: Float): Float = accessNative[Float]
   def double_test(d: Double): Double = accessNative[Double]
   def sum(n: Int) = accessNativeVariadic[Int](n)

   def bad_method(str: Ptr[Byte]): Unit = accessNative[Unit]
   def ibreak(str: Ptr[Byte]): String = accessNative[String]
