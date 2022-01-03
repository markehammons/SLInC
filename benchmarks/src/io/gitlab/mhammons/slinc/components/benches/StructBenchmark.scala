package io.gitlab.mhammons.slinc.components.benches

import scala.util.Random
import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import java.util.Arrays
import scala.annotation.tailrec
import io.gitlab.mhammons.slinc.*
import io.gitlab.mhammons.slinc.benches.repeatInl
import io.gitlab.mhammons.slinc.components.{
   TempAllocator,
   localAllocator,
   reset
}
import jdk.incubator.foreign.{SegmentAllocator, ResourceScope}

@State(Scope.Thread)
@Fork(
  jvmArgsAppend = Array(
    "--add-modules",
    "jdk.incubator.foreign",
    "--enable-native-access",
    "ALL-UNNAMED"
  )
)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.SampleTime))
class StructBenchmark:
   case class div_t(a: Int, b: Int) derives Struct
   // case class div_nested(a: div_t, b: div_t) derives Struct

   @Param(Array("1", "100", "10000"))
   var reps: Int = _

   var rs: ResourceScope = _
   var segAlloc: SegmentAllocator = _
   var memAlloc: SegmentAllocator = _
   // var divNested: Ptr[div_nested] = _
   var divRo: div_t = _

   val b = b_t(4, a_t(3, 2))

   var divSimple: Ptr[div_t] = _

   def div(numerator: Int, denominator: Int)(using SegmentAllocator): div_t =
      bind

   // var divN: div_nested = _

   @Setup(Level.Iteration)
   def setup() =
      rs = ResourceScope.newConfinedScope
      segAlloc = SegmentAllocator.arenaAllocator(rs)
      memAlloc = SegmentAllocator.ofScope(rs)
      given SegmentAllocator = segAlloc
      divRo = div_t(5, 3)
      // divN = div_nested(divRo, divRo)
      divSimple = divRo.serialize
   // divNested = divN.serialize

   @TearDown(Level.Iteration)
   def teardown() =
      rs.close

   @Benchmark
   def allocateSimple =
      scope {
         repeatInl(divRo.serialize, reps)
      }

   @Benchmark
   def allocateTemporary =
      given SegmentAllocator = localAllocator
      try repeatInl(summon[SegmentAllocator].allocate(8, 4), reps)
      finally reset()

   // @Benchmark
   // def allocate2Nested =
   //    given SegmentAllocator = segAlloc
   //    repeat(divN.serialize, reps)

   @Benchmark
   def access2Simple =
      repeatInl(!divSimple.partial.a, reps)
   // @Benchmark
   // def access2Nested =
   //    repeat(!divNested.partial.a, reps)

   @Benchmark
   def accessROSimple =
      repeatInl(divRo.a, reps)

   @Benchmark
   def passInto =
      repeatInl(LibTest.slinc_test_modify(b), reps)
