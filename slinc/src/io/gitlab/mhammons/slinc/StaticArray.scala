package io.gitlab.mhammons.slinc

import scala.reflect.ClassTag
import scala.compiletime.ops.int.<
import components.{Deserializer, deserializerOf, Serializer, serializerOf}
import javax.naming.spi.DirStateFactory.Result
import components.NativeInfo
import jdk.incubator.foreign.{MemoryLayout, MemorySegment, MemoryAddress}

type InboundsProof[Size <: Singleton & Int, Index <: Singleton & Int, Result] =
   (Index < Size =:= true) ?=> Result
class StaticArray[T, Size <: Singleton & Int](
    array: Array[T],
    val size: Int
):
   def apply[Idx <: Singleton & Int](
       index: Idx
   ): InboundsProof[Size, Idx, T] = array(index)

   def apply(index: Int) = array(index)

   def update[Idx <: Singleton & Int](
       index: Idx,
       t: T
   ): InboundsProof[Size, Idx, Unit] = array(index) = t
   def update(index: Int, t: T) = array(index) = t
   def underlying = array

object StaticArray:
   def apply[T: ClassTag: NativeInfo, Size <: Singleton & Int: ValueOf] =
      new StaticArray[T, Size](
        Array.ofDim[T](valueOf[Size]),
        valueOf[Size]
      )

   given [T: NativeInfo, Size <: Singleton & Int: ValueOf]
       : NativeInfo[StaticArray[T, Size]] with
      val layout =
         MemoryLayout.sequenceLayout(valueOf[Size], NativeInfo[T].layout)
      val carrierType = classOf[MemorySegment]

   given [
       A: ClassTag: NativeInfo: Deserializer,
       B <: Singleton & Int: ValueOf
   ]: Deserializer[StaticArray[A, B]] =
      new Deserializer[StaticArray[A, B]]:
         def from(
             memoryAddress: MemoryAddress,
             offset: Long
         ): StaticArray[A, B] =
            val s = StaticArray[A, B]
            val len = valueOf[B]
            var i = 0
            val ni = NativeInfo[A]
            while i < len do
               s(i) = deserializerOf[A].from(
                 memoryAddress,
                 offset + (i * ni.layout.byteSize)
               )
               i += 1
            s

   given [A, B <: Singleton & Int](using
       Serializer[A],
       NativeInfo[A],
       ValueOf[B]
   ): Serializer[StaticArray[A, B]] =
      new Serializer[StaticArray[A, B]]:
         def into(
             staticArray: StaticArray[A, B],
             memoryAddress: MemoryAddress,
             offset: Long
         ) =
            val nativeInfo = NativeInfo[A]
            val len = valueOf[B]
            var i = 0
            while i < staticArray.size do
               serializerOf[A].into(
                 staticArray(i),
                 memoryAddress,
                 offset + (nativeInfo.layout.byteSize * i)
               )
               i += 1
