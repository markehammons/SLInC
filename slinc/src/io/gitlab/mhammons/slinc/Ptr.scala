package io.gitlab.mhammons.slinc

import scala.quoted.*
import scala.util.chaining.*
import scala.compiletime.summonFrom
import components.ffi.{Addressable, Address, Layout, Scope, Segment}
import components.{
   Reader,
   Writer,
   NativeInfo,
   summonOrError,
   readerOf,
   infoOf,
   layoutOf,
   Informee,
   Readee,
   Writee,
   writerOf,
   Emigrator,
   Allocatee
}
import scala.reflect.ClassTag
import scala.jdk.CollectionConverters.*
import io.gitlab.mhammons.slinc.components.Immigrator
import io.gitlab.mhammons.slinc.components.Exporter

/** Describes an address to native memory, and what it's pointing to.
  * @tparam A
  *   The datatype that can be extracted from this pointer
  * @param memoryAddress
  *   The address this pointer is wrapping
  * @param offset
  *   Where the data [[A]] is relative to the pointer's address
  */
class Ptr[A](
    memoryAddress: Address,
    offset: Long,
    map: => Map[String, Any] = Map.empty
) extends Selectable:
   lazy val myMap = map

   /** Dereferences this pointer, producing A
     * @return
     *   A
     * @example
     * ```scala
     * val x: Ptr[Int] = ???
     * val y: Int = !x
     * ```
     */
   def `unary_!` : Readee[A, A] =
      deref

   def deref: Readee[A, A] =
      readerOf[A].from(
        memoryAddress,
        offset
      )

   /** Copies data into this pointer
     * @param a
     *   The data you want to copy into native memory
     * @example
     * ```scala
     * val x: Ptr[Int]
     * !x = 5
     * !x == 5 //true
     * ```
     */
   def `unary_!_=`(a: A): Writee[A, Unit] =
      deref = a

   def deref_=(a: A): Writee[A, Unit] =
      writerOf[A].into(a, memoryAddress.asInstanceOf[Address], offset)

   /** Offsets this pointer by a number of elements of size A
     * @param num
     *   The number of elements A to offset this pointer by
     * @return
     *   A new, offset pointer
     */
   def +(num: Long)(using layoutOf: NativeInfo[A]) = new Ptr[A](
     memoryAddress.address.addOffset(layoutOf.layout.byteSize * num),
     offset,
     map
   )

   /** Casts this pointer
     * @tparam B
     *   the new type this pointer will represent
     * @return
     *   A pointer to B
     */
   inline def castTo[B] =
      Ptr[B](memoryAddress, offset)
   def asMemoryAddress = memoryAddress.address.addOffset(offset)
   def selectDynamic(key: String) = myMap(key)

   /** Transform this pointer into an array
     * @param size
     *   The number of elements the pointer is pointing to
     * @return
     *   A scala array with the aforementioned size
     */
   @deprecated("use mkArray instead", "v0.1.1") def toArray(
       size: Int
   )(using Reader[A], NativeInfo[A], ClassTag[A]) =
      val l = NativeInfo[A].layout
      val elemSize = l.byteSize
      var i = 0
      val arr = Array.ofDim[A](size)
      while i < size do
         arr(i) = readerOf[A].from(
           memoryAddress.addOffset(i * elemSize),
           offset
         )
         i += 1
      arr

object Ptr:
   inline def apply[A](
       memoryAddress: Address,
       offset: Long
   ): Ptr[A] =
      summonFrom {
         case _: NativeInfo[A] =>
            genPtr(memoryAddress, offset, infoOf[A].layout).asInstanceOf[Ptr[A]]
         case _ =>
            new Ptr[A](memoryAddress, offset, Map.empty)
      }

   private def genPtr(
       memoryAddress: Address,
       offset: Long,
       memoryLayout: Layout
   ): Ptr[Any] =
      memoryLayout match
         case gl: Layout.Group =>
            new Ptr(
              memoryAddress,
              offset,
              gl.memberLayouts
                 .map(ml =>
                    ml.name.get.pipe(n =>
                       n -> genPtr(
                         memoryAddress,
                         gl.byteOffset(Layout.Path.groupElement(n)),
                         ml
                       )
                    )
                 )
                 .toMap
            )
         case vl: Layout.Value => new Ptr(memoryAddress, offset, Map.empty)
         case _ => new Ptr(memoryAddress, offset, Map.empty)

   /** Null pointer implementation
     */
   class Null[A: NativeInfo: ClassTag] extends Ptr[A](Address.NULL, 0):
      override def deref =
         throw NullPointerException("SLinC Null Ptr attempted dereference")
      override def deref_=(a: A) =
         throw NullPointerException("SLinC Null Ptr attempted value update")
      override def asMemoryAddress = Address.NULL

   extension (a: Ptr[Byte])
      /** Transforms a Ptr[Byte] to a Scala String
        */
      def mkString = a.asMemoryAddress.getUtf8String(0)
      def toByteArray(size: Int) =
         val addr = a.asMemoryAddress
         Segment.ofAddress(addr, layoutOf[Byte].byteSize * size, Scope.globalScope).toByteArray

   extension (a: Ptr[Short])
      def mkArray(size: Int) =
         val addr = a.asMemoryAddress
         Segment.ofAddress(addr, layoutOf[Short].byteSize * size, Scope.globalScope).toShortArray
   extension (a: Ptr[Int])
      def mkArray(size: Int) =
         val addr = a.asMemoryAddress
         Segment.ofAddress(addr, layoutOf[Int].byteSize * size, Scope.globalScope).toIntArray

   extension (a: Ptr[Long])
      def mkArray(size: Int) =
         val addr = a.asMemoryAddress
         Segment.ofAddress(addr, layoutOf[Long].byteSize * size, Scope.globalScope).toLongArray

   extension (a: Ptr[Float])
      def mkArray(size: Int) =
         val addr = a.asMemoryAddress
         Segment.ofAddress(addr, layoutOf[Float].byteSize * size, Scope.globalScope).toFloatArray

   extension (a: Ptr[Double])
      def mkArray(size: Int) =
         val addr = a.asMemoryAddress
         Segment.ofAddress(addr, layoutOf[Double].byteSize * size, Scope.globalScope).toDoubleArray

   extension [A](a: Ptr[A])
      /** Enables partial dereferencing of a Pointer to a Struct type
        * @return
        *   a, but refined with extra struct information
        *
        * @example
        * ```scala
        * case class div_t(quot: Int, rem: Int) derives Struct
        *
        * val a = div_t(5, 6)
        * val rem = scope {
        *   val aPtr = a.serialize
        *   aPtr.partial.quot.deref // only copies quot's data into the jvm
        * }
        * ```
        */
      transparent inline def partial = ${ selectableImpl[A]('a) }
      def mkArray(size: Int)(using NativeInfo[A], ClassTag[A], Reader[A]) =
         val memoryAddress = a.asMemoryAddress
         val l = NativeInfo[A].layout
         val elemSize = l.byteSize
         var i = 0
         val arr = Array.ofDim[A](size)
         while i < size do
            arr(i) = readerOf[A].from(
              memoryAddress.addOffset(i * elemSize),
              0
            )
            i += 1
         arr

   private def selectableImpl[A: Type](nptr: Expr[Ptr[A]])(using Quotes) =
      val typeInfo = TypeInfo[A]
      produceDualRefinement(typeInfo) match
         case '[refinement] =>
            val layout = Expr.summonOrError[NativeInfo[A]]
            '{
               val l = $layout.layout
               val memSegmnt = $nptr.asMemoryAddress
               $nptr.asInstanceOf[refinement]
            }

   private def produceDualRefinement(typeInfo: TypeInfo)(using
       Quotes
   ): Type[?] =
      import quotes.reflect.*
      typeInfo match
         case ProductInfo(name, members, '[a]) =>
            members
               .foldLeft(TypeRepr.of[Ptr[a]]) { (accum, m) =>
                  Refinement(
                    accum,
                    m.name,
                    produceDualRefinement(m).pipe { case '[a] =>
                       TypeRepr.of[a]
                    }
                  )
               }
               .asType
         case PrimitiveInfo(name, '[a]) =>
            Type.of[Ptr[a]]
         case PtrInfo(name, typeInfo, '[a]) =>
            produceDualRefinement(typeInfo).pipe { case '[b] =>
               Type.of[Ptr[b]]
            }

   given gen[A <: Ptr[?]]: NativeInfo[A] =
      summon[NativeInfo[String]].asInstanceOf[NativeInfo[A]]

   given [A]: Emigrator[Ptr[A]] with
      def apply(a: Ptr[A]): Allocatee[Any] = a.asMemoryAddress

   given [A]: Immigrator[Ptr[A]] = a => Ptr[A](a.asInstanceOf[Address], 0)

   given [A, P <: Ptr[A]](using NativeInfo[A]): Reader[P] with
      def from(
          memoryAddress: Address,
          offset: Long
      ): P =
         Ptr[A](memoryAddress.address.getAddress(offset), 0).asInstanceOf[P]

   private val ptrSerializer = new Writer[Ptr[Any]]:
      def into(ptr: Ptr[Any], memoryAddress: Address, offset: Long) =
         memoryAddress.setAddress(offset, ptr.asMemoryAddress)

   given [A]: Writer[Ptr[A]] =
      ptrSerializer.asInstanceOf[Writer[Ptr[A]]]

   given [A]: Exporter[Ptr[A]] = Exporter.derive[Ptr[A]]
