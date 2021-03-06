package io.gitlab.mhammons.cstd.benches

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import io.gitlab.mhammons.slinc.*
import io.gitlab.mhammons.cstd.*
import scala.util.chaining.*
import org.openjdk.jmh.infra.Blackhole
import scala.util.Random

@State(Scope.Thread)
@Fork(
  jvmArgsAppend = Array(
    "--add-modules=jdk.incubator.foreign",
    "--enable-native-access=ALL-UNNAMED",
    "-Djmh.blackhole.autoDetect=true"
  )
)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class StdLibBench:
   val arrayToSort = Array.fill(10000)(Random.nextInt)
   val fn = (a: Ptr[Any], b: Ptr[Any]) =>
      val aVal = !a.castTo[Int]
      val bVal = !b.castTo[Int]

      if aVal < bVal then -1
      else if aVal == bVal then 0
      else 1

   val fnPtr = globalScope { fn.encode }
   @Benchmark
   def abs =
      StdLib.abs(10)

   @Benchmark
   def scalaAbs(blackhole: Blackhole) =
      blackhole.consume(Math.abs(10))

   @OutputTimeUnit(TimeUnit.MILLISECONDS)
   @Benchmark
   def qsort =
      scope {
         val arrPtr = arrayToSort.encode
         StdLib.qsort(
           arrPtr.castTo[Any],
           SizeT.fromIntOrFail(arrayToSort.length),
           sizeOf[Int],
           fn.encode
         )

         arrPtr.toArray(arrayToSort.length)
      }

   // todo: Needs QoL for copying data into pre-existing buffers
   // def qsortNonAllocating

   @OutputTimeUnit(TimeUnit.MILLISECONDS)
   @Benchmark
   def scalaSort = arrayToSort.sorted

   @Benchmark
   def rand = StdLib.rand()

   @Benchmark
   def scalaRand = Random.nextInt()

   @Benchmark
   def div =
      StdLib.div(14, 5)

   @Benchmark
   def scalaDiv = (14 / 5, 14 % 5)

   @Benchmark
   def getEnv =
      scope {
         StdLib.getenv("HOME".encode)
      }

   @Benchmark
   def scalaGetEnv =
      System.getenv("HOME")

   @Benchmark
   def atol =
      scope {
         StdLib.atol("15".encode)
      }

   @Benchmark
   def scalaAtol =
      "15".toLong
