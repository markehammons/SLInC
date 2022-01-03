package io.gitlab.mhammons.slinc.components

import jdk.incubator.foreign.{
   MemoryAccess,
   MemorySegment,
   MemoryAddress,
   ValueLayout
}

type Serializee[A, B] = Serializer[A] ?=> B
def serializerOf[A]: Serializee[A, Serializer[A]] = summon[Serializer[A]]

//todo: rename to encoder
trait Serializer[A]:
   def into(a: A, memoryAddress: MemoryAddress, offset: Long): Unit

object Serializer:
   given Serializer[Int] with
      def into(a: Int, memoryAddress: MemoryAddress, offset: Long) =
         MemoryAccess.setIntAtOffset(
           MemorySegment.globalNativeSegment,
           memoryAddress.toRawLongValue + offset,
           a
         )

   given Serializer[Long] with
      def into(a: Long, memoryAddress: MemoryAddress, offset: Long) =
         MemoryAccess.setLongAtOffset(
           MemorySegment.globalNativeSegment,
           memoryAddress.toRawLongValue + offset,
           a
         )

   given Serializer[Float] with
      def into(a: Float, memoryAddress: MemoryAddress, offset: Long) =
         MemoryAccess.setFloatAtOffset(
           MemorySegment.globalNativeSegment,
           memoryAddress.toRawLongValue + offset,
           a
         )

   given Serializer[Double] with
      def into(a: Double, memoryAddress: MemoryAddress, offset: Long) =
         MemoryAccess.setDoubleAtOffset(
           MemorySegment.globalNativeSegment,
           memoryAddress.toRawLongValue + offset,
           a
         )

   given Serializer[Short] with
      def into(a: Short, memoryAddress: MemoryAddress, offset: Long) =
         MemoryAccess.setShortAtOffset(
           MemorySegment.globalNativeSegment,
           memoryAddress.toRawLongValue + offset,
           a
         )

   given Serializer[Boolean] with
      def into(a: Boolean, memoryAddress: MemoryAddress, offset: Long) =
         MemoryAccess.setByteAtOffset(
           MemorySegment.globalNativeSegment,
           memoryAddress.toRawLongValue + offset,
           if a then 1 else 0
         )

   given Serializer[Char] with
      def into(a: Char, memoryAddress: MemoryAddress, offset: Long) =
         MemoryAccess.setCharAtOffset(
           MemorySegment.globalNativeSegment,
           memoryAddress.toRawLongValue + offset,
           a
         )
   given Serializer[Byte] with
      def into(a: Byte, memoryAddress: MemoryAddress, offset: Long) =
         MemoryAccess.setByteAtOffset(
           MemorySegment.globalNativeSegment,
           memoryAddress.toRawLongValue + offset,
           a
         )

   given [A](using Serializer[A], NativeInfo[A]): Serializer[Array[A]] with
      def into(array: Array[A], memoryAddress: MemoryAddress, offset: Long) =
         var i = 0
         while i < array.length do
            serializerOf[A].into(
              array(i),
              memoryAddress,
              offset + (NativeInfo[A].layout.byteSize * i)
            )
            i += 1
