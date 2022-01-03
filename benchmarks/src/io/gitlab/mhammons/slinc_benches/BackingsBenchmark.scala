package io.gitlab.mhammons.slinc_benches

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import scala.util.Random
import io.gitlab.mhammons.slinc.Struct
import jdk.incubator.foreign.ResourceScope
import jdk.incubator.foreign.CLinker.C_INT
import jdk.incubator.foreign.SegmentAllocator
import jdk.incubator.foreign.MemoryAccess
import io.gitlab.mhammons.polymorphics.VarHandleHandler

// class Str:

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
class BackingsBenchmark:
   val resourceScope = ResourceScope.newConfinedScope
   val segAlloc = SegmentAllocator.arenaAllocator(resourceScope)
   val memsegment = segAlloc.allocate(C_INT)
   val varhandle = C_INT.varHandle(classOf[Int])
   // @Benchmark
   // def mapBench(str: Str) =
   //    val key = str.keys.next
   //    repeatInl(str.map(key), 10000)
   // @Benchmark
   // def minimalPerfectHashTable(str: Str) =
   //    val key = str.keys.next
   //    repeatInl(str.mpht(key), 10000)

   // @Benchmark
   // def minimalPowerPerfectHashtable(str: Str) =
   //    val key = str.keys.next
   //    repeatInl(str.mfpht(key), 10000)

// @Benchmark
// def summonVarhandleFromLayout(str: Str) = str.layout.varHandle(classOf[Int])

   @Benchmark
   def useVarHandle =
      repeatInl(VarHandleHandler.get(varhandle, memsegment), 100)

   @Benchmark
   def useMemAccessOffset =
      repeatInl(MemoryAccess.getIntAtOffset(memsegment, 0), 100)

   @Benchmark
   def useMemAccess =
      repeatInl(MemoryAccess.getInt(memsegment), 100)
