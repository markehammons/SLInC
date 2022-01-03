package io.gitlab.mhammons.slinc

import scala.quoted.*
import jdk.incubator.foreign.{
   SegmentAllocator,
   MemoryAddress,
   MemorySegment,
   MemoryLayout
}, MemoryLayout.PathElement
import io.gitlab.mhammons.slinc.components.{
   NativeInfo,
   Serializer,
   Deserializer,
   TypeInfo,
   PrimitiveInfo,
   ProductInfo,
   PtrInfo,
   segAlloc,
   summonOrError
}
import scala.util.chaining.*
import io.gitlab.mhammons.slinc.components.Immigrator
import io.gitlab.mhammons.slinc.components.Emigrator

trait Struct[A <: Product]
    extends NativeInfo[A],
      Immigrator[A],
      Emigrator[A],
      Serializer[A],
      Deserializer[A]

object Struct:
   inline given derived[A <: Product]: Struct[A] = ${
      derivedImpl[A]
   }

   private def derivedImpl[A <: Product: Type](using Quotes) =
      import quotes.reflect.*
      val typeInfo = TypeInfo[A]
      '{
         // helps prevent recursion issues on Struct instantiation
         given l: NativeInfo[A] with
            val layout = ${ fromTypeInfo(typeInfo) }
            val carrierType = classOf[MemorySegment]

         given s: Serializer[A] with
            def into(a: A, memoryAddress: MemoryAddress, offset: Long): Unit =
               ${
                  serializerFromTypeInfo(
                    'a,
                    'memoryAddress,
                    'offset,
                    '{ l.layout },
                    Nil,
                    typeInfo
                  )
               }

         given d: Deserializer[A] with
            def from(memoryAddress: MemoryAddress, offset: Long) =
               ${
                  deserializerFromTypeInfo(
                    'memoryAddress,
                    Nil,
                    typeInfo
                  )
               }.asInstanceOf[A]

         new Struct[A]:
            val carrierType = classOf[MemorySegment]
            val layout = l.layout

            def into(a: A, memoryAddress: MemoryAddress, offset: Long): Unit =
               s.into(a, memoryAddress, offset)

            def from(memoryAddress: MemoryAddress, offset: Long) =
               d.from(memoryAddress, offset)

            def apply(a: A) =
               val segment = segAlloc.allocate(layout)
               into(a, segment.address, 0)
               segment

            def apply(a: Any) =
               from(a.asInstanceOf[MemorySegment].address, 0)

      }

   private def fromTypeInfo(typeInfo: TypeInfo)(using
       Quotes
   ): Expr[MemoryLayout] =
      typeInfo match
         case PrimitiveInfo(name, '[a]) =>
            '{
               ${ Expr.summonOrError[NativeInfo[a]] }.layout.withName(${
                  Expr(name)
               })
            }
         case PtrInfo(name, _, '[a]) =>
            '{
               ${ Expr.summonOrError[NativeInfo[a]] }.layout.withName(${
                  Expr(name)
               })
            }
         case ProductInfo(name, members, _) =>
            '{
               MemoryLayout
                  .structLayout(${
                     members.map(fromTypeInfo).pipe(Expr.ofSeq)
                  }*)
                  .withName(${ Expr(name) })
            }

   private def deserializerFromTypeInfo[A: Type](
       memorySegmentExpr: Expr[MemoryAddress],
       path: Seq[Expr[PathElement]],
       typeInfo: TypeInfo
   )(using q: Quotes): Expr[Any] =
      import quotes.reflect.*
      typeInfo match
         case PrimitiveInfo(name, '[a]) =>
            val updatedPath = Expr.ofSeq(path :+ '{
               PathElement.groupElement(${ Expr(name) })
            })
            val info = Expr.summonOrError[NativeInfo[A]]
            '{
               ${ Expr.summonOrError[Deserializer[a]] }.from(
                 $memorySegmentExpr,
                 $info.layout.byteOffset($updatedPath*)
               )
            }
         case ProductInfo(name, members, '[a]) =>
            val updatedPath =
               if name.isEmpty then path
               else path :+ '{ PathElement.groupElement(${ Expr(name) }) }
            Apply(
              Select(
                New(TypeTree.of[a]),
                TypeRepr.of[a].typeSymbol.primaryConstructor
              ),
              members
                 .map(m =>
                    deserializerFromTypeInfo[A](
                      memorySegmentExpr,
                      updatedPath,
                      m
                    ).asTerm
                 )
                 .toList
            ).asExpr
         case PtrInfo(name, _, t) =>
            deserializerFromTypeInfo(
              memorySegmentExpr,
              path,
              PrimitiveInfo(name, t)
            )

   private def serializerFromTypeInfo(
       a: Expr[?],
       memoryAddress: Expr[MemoryAddress],
       offset: Expr[Long],
       layout: Expr[MemoryLayout],
       path: Seq[Expr[PathElement]],
       typeInfo: TypeInfo
   )(using Quotes): Expr[Unit] =
      import quotes.reflect.*
      typeInfo match
         case PrimitiveInfo(name, '[a]) =>
            val to = Expr.summonOrError[Serializer[a]]
            val pathExpr = Expr.ofSeq(path)
            '{
               $to.into(
                 ${ a.asExprOf[a] },
                 $memoryAddress,
                 $layout.byteOffset($pathExpr*) + $offset
               )
            }
         case ProductInfo(name, members, '[a]) =>
            val aTerm = a.asTerm
            val aMembers =
               TypeRepr.of[a].typeSymbol.caseFields.map(s => s.name -> s).toMap
            val memberSelect = members.map { m =>
               val updatedPath = path :+ '{
                  PathElement.groupElement(${ Expr(m.name) })
               }
               Select(aTerm, aMembers(m.name)).asExpr.pipe(
                 serializerFromTypeInfo(
                   _,
                   memoryAddress,
                   offset,
                   layout,
                   updatedPath,
                   m
                 )
               )
            }
            Expr.block(memberSelect.toList, '{}).tap(_.show.tap(report.info))

         // pointer handling is exactly the same as primitive handling here.
         case PtrInfo(name, _, t) =>
            serializerFromTypeInfo(
              a,
              memoryAddress,
              offset,
              layout,
              path,
              PrimitiveInfo(name, t)
            )
