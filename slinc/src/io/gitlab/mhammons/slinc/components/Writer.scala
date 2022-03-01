package io.gitlab.mhammons.slinc.components

import ffi.{Address, Segment, Scope}
import scala.compiletime.erasedValue

type Writee[A, B] = Writer[A] ?=> B
def writerOf[A]: Writee[A, Writer[A]] = summon[Writer[A]]
def write[A](
    a: A,
    memoryAddress: Address,
    offset: Long
): Writee[A, Unit] = writerOf[A].into(a, memoryAddress, offset)

trait Writer[A]:
   def into(a: A, memoryAddress: Address, offset: Long): Unit
   def contramap[B](fn: B => A): Writer[B] =
      val orig = this
      new Writer[B]:
         def into(a: B, memoryAddress: Address, offset: Long) =
            orig.into(fn(a), memoryAddress, offset)
object Writer:
   given Writer[Int] with
      def into(a: Int, memoryAddress: Address, offset: Long) =
         memoryAddress.setInt(offset, a)

   given Writer[Long] with
      def into(a: Long, memoryAddress: Address, offset: Long) =
         memoryAddress.setLong(offset, a)
   given Writer[Float] with
      def into(a: Float, memoryAddress: Address, offset: Long) =
         memoryAddress.setFloat(offset, a)

   given Writer[Double] with
      def into(a: Double, memoryAddress: Address, offset: Long) =
         memoryAddress.setDouble(offset, a)

   given Writer[Short] with
      def into(a: Short, memoryAddress: Address, offset: Long) =
         memoryAddress.setShort(offset, a)

   given Writer[Boolean] with
      def into(a: Boolean, memoryAddress: Address, offset: Long) =
         memoryAddress.setByte(offset, if a then 1 else 0)

   given Writer[Char] with
      def into(a: Char, memoryAddress: Address, offset: Long) =
         memoryAddress.setShort(offset, a.toShort)

   given Writer[Byte] with
      def into(a: Byte, memoryAddress: Address, offset: Long) =
         memoryAddress.setByte(offset, a)

   private inline def arrayToMemsegment[A](array: Array[A]) = inline array match
      case arr: Array[Byte]   => Segment.ofArray(arr)
      case arr: Array[Short]  => Segment.ofArray(arr)
      case arr: Array[Int]    => Segment.ofArray(arr)
      case arr: Array[Long]   => Segment.ofArray(arr)
      case arr: Array[Float]  => Segment.ofArray(arr)
      case arr: Array[Double] => Segment.ofArray(arr)

   private inline def specializedEncoderCopy[A](
       array: Array[A],
       memoryAddress: Address,
       offset: Long
   )(using NativeInfo[A]) =
      Segment
         .ofAddress(
           memoryAddress.addOffset(offset),
           layoutOf[A].byteSize * array.length,
           Scope.globalScope
         )
         .copyFrom(arrayToMemsegment(array))

   given byteArr: Writer[Array[Byte]] = specializedEncoderCopy(_, _, _)

   given shortArr: Writer[Array[Short]] = specializedEncoderCopy(_, _, _)

   given intArr: Writer[Array[Int]] = specializedEncoderCopy(_, _, _)

   given longArr: Writer[Array[Long]] = specializedEncoderCopy(_, _, _)

   given floatArr: Writer[Array[Float]] = specializedEncoderCopy(_, _, _)

   given doubleArr: Writer[Array[Double]] = specializedEncoderCopy(_, _, _)

   given [A](using Writer[A], NativeInfo[A]): Writer[Array[A]] with
      def into(array: Array[A], memoryAddress: Address, offset: Long) =
         var i = 0
         while i < array.length do
            writerOf[A].into(
              array(i),
              memoryAddress,
              offset + (NativeInfo[A].layout.byteSize * i)
            )
            i += 1
