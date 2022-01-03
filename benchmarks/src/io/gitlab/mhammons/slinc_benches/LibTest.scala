package io.gitlab.mhammons.slinc.components.benches

import io.gitlab.mhammons.slinc.*
import io.gitlab.mhammons.slinc.components.NativeInfo

case class a_t(a: Int, b: Int) derives Struct
case class b_t(c: Int, d: a_t) derives Struct

case class c_t(p: Ptr[a_t]) derives Struct

object LibTest
    extends Library(Location.Local("../../../../slinc/test/native/libtest.so")):

   def slinc_test_modify(b_t: b_t): b_t = bind

   val x: Ptr[c_t] = ???

   val r = !x.partial.p
