package io.gitlab.mhammons.slinc.components

import jdk.incubator.foreign.{MemorySegment, MemoryAccess, MemoryAddress}
import scala.util.chaining.*

type Deserializee[A, B] = Deserializer[A] ?=> B
def deserializerOf[A]: Deserializee[A, Deserializer[A]] =
   summon[Deserializer[A]]
//todo: rename to decoder
trait Deserializer[A]:
   def from(memoryAddress: MemoryAddress, offset: Long): A
object Deserializer:
   given Deserializer[Int] with
      def from(memoryAddress: MemoryAddress, offset: Long) =
         MemoryAccess.getIntAtOffset(
           MemorySegment.globalNativeSegment,
           memoryAddress.toRawLongValue + offset
         )
   given Deserializer[Float] with
      def from(memoryAddress: MemoryAddress, offset: Long) =
         MemoryAccess.getFloatAtOffset(
           MemorySegment.globalNativeSegment,
           memoryAddress.toRawLongValue + offset
         )

   given Deserializer[Long] with
      def from(memoryAddress: MemoryAddress, offset: Long) =
         MemoryAccess.getLongAtOffset(
           MemorySegment.globalNativeSegment,
           memoryAddress.toRawLongValue + offset
         )

   given Deserializer[Short] with
      def from(memoryAddress: MemoryAddress, offset: Long) =
         MemoryAccess.getShortAtOffset(
           MemorySegment.globalNativeSegment,
           memoryAddress.toRawLongValue + offset
         )

   given Deserializer[Byte] with
      def from(memoryAddress: MemoryAddress, offset: Long) =
         MemoryAccess.getByteAtOffset(
           MemorySegment.globalNativeSegment,
           memoryAddress.toRawLongValue + offset
         )
