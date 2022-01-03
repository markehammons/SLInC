package io.gitlab.mhammons.slinc

import scala.quoted.*
import scala.util.chaining.*
import jdk.incubator.foreign.{
   MemorySegment,
   MemoryLayout,
   MemoryAddress,
   MemoryAccess,
   CLinker,
   ResourceScope,
   GroupLayout,
   ValueLayout
}, MemoryLayout.PathElement
import components.{
   Deserializer,
   Serializer,
   NativeInfo,
   summonOrError,
   deserializerOf,
   infoOf,
   Informee,
   Deserializee,
   Serializee,
   serializerOf,
   Emigrator,
   Allocatee
}
import scala.reflect.ClassTag
import scala.jdk.CollectionConverters.*
import io.gitlab.mhammons.slinc.components.Immigrator

class Ptr[A](
    memoryAddress: MemoryAddress,
    offset: Long,
    map: => Map[String, Any] = Map.empty
) extends Selectable:
   lazy val myMap = map
   def `unary_!` : Deserializee[A, A] =
      deref

   def deref: Deserializee[A, A] =
      deserializerOf[A].from(
        memoryAddress,
        offset
      )

   def `unary_!_=`(a: A): Serializee[A, Unit] =
      deref = a

   def deref_=(a: A): Serializee[A, Unit] =
      serializerOf[A].into(a, memoryAddress, offset)

   def +(plus: Long)(using layoutOf: NativeInfo[A]) = new Ptr[A](
     memoryAddress.addOffset(layoutOf.layout.byteSize * plus),
     offset,
     map
   )
   def asMemoryAddress = memoryAddress
   def selectDynamic(key: String) = myMap(key)
   def toArray(size: Int)(using Deserializer[A], NativeInfo[A], ClassTag[A]) =
      val l = NativeInfo[A].layout
      val elemSize = l.byteSize
      var i = 0
      val arr = Array.ofDim[A](size)
      while i < size do
         arr(i) = deserializerOf[A].from(
           memoryAddress.addOffset(i * elemSize),
           offset
         )
         i += 1
      arr

   def rescope(using ResourceScope) =
      if memoryAddress.scope == ResourceScope.globalScope then
         new Ptr[A](
           memoryAddress.asSegment(1, summon[ResourceScope]).address,
           offset,
           map
         )
      else
         throw new IllegalStateException(
           "This pointer already belongs to another scope, cannot be moved"
         )

object Ptr:
   def apply[A](
       memoryAddress: MemoryAddress,
       offset: Long
   ): Informee[A, Ptr[A]] =
      genPtr(memoryAddress, offset, infoOf[A].layout).asInstanceOf[Ptr[A]]

   private def genPtr(
       memoryAddress: MemoryAddress,
       offset: Long,
       memoryLayout: MemoryLayout
   ): Ptr[Any] =
      memoryLayout match
         case gl: GroupLayout =>
            new Ptr(
              memoryAddress,
              offset,
              gl.memberLayouts.asScala
                 .map(ml =>
                    ml.name.get.pipe(n =>
                       n -> genPtr(
                         memoryAddress,
                         gl.byteOffset(PathElement.groupElement(n)),
                         ml
                       )
                    )
                 )
                 .toMap
            )
         case vl: ValueLayout => new Ptr(memoryAddress, offset, Map.empty)

   class Null[A: NativeInfo: ClassTag] extends Ptr[A](MemoryAddress.NULL, 0):
      override def deref =
         throw NullPointerException("SLinC Null Ptr attempted dereference")
      override def deref_=(a: A) =
         throw NullPointerException("SLinC Null Ptr attempted value update")
      override def asMemoryAddress = MemoryAddress.NULL
   extension [A](a: Ptr[A])
      transparent inline def partial = ${ selectableImpl[A]('a) }

   def selectableImpl[A: Type](nptr: Expr[Ptr[A]])(using Quotes) =
      val typeInfo = TypeInfo[A]
      produceDualRefinement(typeInfo) match
         case '[refinement] =>
            val layout = Expr.summonOrError[NativeInfo[A]]
            '{
               val l = $layout.layout
               val memSegmnt = $nptr.asMemoryAddress
               $nptr.asInstanceOf[refinement]
            }

   def produceDualRefinement(typeInfo: TypeInfo)(using Quotes): Type[?] =
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

   given [A](using NativeInfo[A]): Emigrator[Ptr[A]] with
      def apply(a: Ptr[A]): Allocatee[Any] = a.asMemoryAddress

   given [A]: Informee[A, Immigrator[Ptr[A]]] = a =>
      Ptr[A](a.asInstanceOf[MemoryAddress], 0)

   given [A, P <: Ptr[A]](using NativeInfo[A]): Deserializer[P] with
      def from(
          memoryAddress: MemoryAddress,
          offset: Long
      ): P =
         val address = MemoryAccess.getAddressAtOffset(
           MemorySegment.globalNativeSegment,
           memoryAddress.toRawLongValue + offset
         )

         Ptr[A](address, 0).asInstanceOf[P]

   private val ptrSerializer = new Serializer[Ptr[Any]]:
      def into(ptr: Ptr[Any], memoryAddress: MemoryAddress, offset: Long) =
         MemoryAccess.setAddressAtOffset(
           MemorySegment.globalNativeSegment,
           memoryAddress.toRawLongValue + offset,
           ptr.asMemoryAddress
         )

   given [A]: Serializer[Ptr[A]] =
      ptrSerializer.asInstanceOf[Serializer[Ptr[A]]]
