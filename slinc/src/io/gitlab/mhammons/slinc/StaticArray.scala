package io.gitlab.mhammons.slinc

import scala.reflect.ClassTag
import scala.compiletime.ops.int.<
import components.{Reader, readerOf, Writer, writerOf}
import javax.naming.spi.DirStateFactory.Result
import components.NativeInfo
import components.ffi.{Layout, Segment, Address, Addressable}

type InboundsProof[Size <: Singleton & Int, Index <: Singleton & Int, Result] =
   (Index < Size =:= true) ?=> Result

/** Indicates a C-fixed size array for use in structs
  * @tparam T
  *   the type the Static Array contains
  * @tparam Size
  *   The size of the static array, knowable at compile time
  */
class StaticArray[T, Size <: Singleton & Int] private (
    array: Array[T],
    val size: Int
):
   /** type level indexing function
     * @tparam Idx
     *   the singleton int index you want.
     * @param index
     *   the index (type Idx) that you want
     * @return
     *   An element T if it can be proven at compile time that your index is not
     *   greater than the size of the array
     */
   def apply[Idx <: Singleton & Int](
       index: Idx
   ): InboundsProof[Size, Idx, T] = array(index)

   /** Runtime indexing function
     * @param index
     *   The index in the array to access
     * @return
     *   T if the index is in bounds, an out of bounds exception otherwise.
     */
   def apply(index: Int) = array(index)

   /** type level update function
     * @tparam Idx
     *   the singleton int index you want to update
     * @param index
     *   the index you want to update
     * @param t
     *   The value to copy into the array
     */
   def update[Idx <: Singleton & Int](
       index: Idx,
       t: T
   ): InboundsProof[Size, Idx, Unit] = array(index) = t

   /** Runtime update function
     * @param index
     *   The array position you wish to update
     * @param t
     *   The value you wish to copy into the array
     */
   def update(index: Int, t: T) = array(index) = t
   def underlying = array

object StaticArray:
   /** creates a new static array
     * @tparam T
     *   the element type of the array
     * @tparam Size
     *   the size of the array as an int literal type
     * @return
     *   An array of size Size.
     */
   def apply[T: ClassTag: NativeInfo, Size <: Singleton & Int: ValueOf] =
      new StaticArray[T, Size](
        Array.ofDim[T](valueOf[Size]),
        valueOf[Size]
      )

   given [T: NativeInfo, Size <: Singleton & Int: ValueOf]
       : NativeInfo[StaticArray[T, Size]] with
      val layout =
         Layout.sequenceLayout(valueOf[Size], NativeInfo[T].layout)

   given [
       A: ClassTag: NativeInfo: Reader,
       B <: Singleton & Int: ValueOf
   ]: Reader[StaticArray[A, B]] =
      new Reader[StaticArray[A, B]]:
         def from(
             memoryAddress: Address,
             offset: Long
         ): StaticArray[A, B] =
            val s = StaticArray[A, B]
            val len = valueOf[B]
            var i = 0
            val ni = NativeInfo[A]
            while i < len do
               s(i) = readerOf[A].from(
                 memoryAddress,
                 offset + (i * ni.layout.byteSize)
               )
               i += 1
            s

   given [A, B <: Singleton & Int](using
       Writer[A],
       NativeInfo[A],
       ValueOf[B]
   ): Writer[StaticArray[A, B]] =
      new Writer[StaticArray[A, B]]:
         def into(
             staticArray: StaticArray[A, B],
             memoryAddress: Address,
             offset: Long
         ) =
            val nativeInfo = NativeInfo[A]
            val len = valueOf[B]
            var i = 0
            while i < staticArray.size do
               writerOf[A].into(
                 staticArray(i),
                 memoryAddress,
                 offset + (nativeInfo.layout.byteSize * i)
               )
               i += 1
