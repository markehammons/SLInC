package io.gitlab.mhammons.slinc

import scala.quoted.*
import components.ffi.{Layout, Address, Segment, Scope, Allocator, Addressable}
import io.gitlab.mhammons.slinc.components.{
   NativeInfo,
   Writer,
   Reader,
   Exporter,
   Allocatee,
   segAlloc,
   summonOrError
}
import scala.util.chaining.*
import io.gitlab.mhammons.slinc.components.Immigrator
import io.gitlab.mhammons.slinc.components.Emigrator

/** A typeclass used to allow product types to be used as parameters in C
  * bindings
  * @tparam A
  *   A product type
  * @example
  * ```scala
  * case class div_t(quot: Int, rem: Int) derives Struct
  * ```
  */
trait Struct[A <: Product]
    extends NativeInfo[A],
      Immigrator[A],
      Emigrator[A],
      Writer[A],
      Reader[A],
      Exporter[A]

object Struct:
   inline given derived[A <: Product]: Struct[A] = ${
      derivedImpl[A]
   }

   private def derivedImpl[A <: Product: Type](using Quotes, Type[Layout.Path], Type[Layout.Group]) =
      import quotes.reflect.*
      val typeInfo = TypeInfo[A]
      '{
         // helps prevent recursion issues on Struct instantiation
         given l: NativeInfo[A] with
            val layout = ${ fromTypeInfo(typeInfo) }

         given s: Writer[A] with
            def into(a: A, memoryAddress: Address, offset: Long): Unit =
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

         given d: Reader[A] with
            def from(memoryAddress: Address, offset: Long) =
               ${
                  deserializerFromTypeInfo[A](
                    '{memoryAddress.address},
                    Nil,
                    typeInfo
                  )
               }.asInstanceOf[A]

         new Struct[A]:
            val layout = l.layout

            def into(a: A, memoryAddress: Address, offset: Long): Unit =
               s.into(a, memoryAddress, offset)

            def from(memoryAddress: Address, offset: Long) =
               d.from(memoryAddress, offset)

            def apply(a: A): Allocatee[Any] =
               val segment = segAlloc.allocateSegment(layout)
               into(a, segment.address, 0)
               segment

            def exportValue(a: A)(using Scope, Allocator) =
               val segment = segAlloc.allocate(layout)
               into(a, segment.address, 0)
               segment.address

            def apply(a: Any) =
               from(a.asInstanceOf[Segment].address, 0)

      }

   private def fromTypeInfo(typeInfo: TypeInfo)(using
       Quotes, Type[Layout.Group]
   ): Expr[Layout] =
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
               Layout
                  .structLayout(${
                     members.map(fromTypeInfo).pipe(Expr.ofSeq)
                  }*)
                  .withName(${ Expr(name) })
            }

   private def deserializerFromTypeInfo[A](
       memorySegmentExpr: Expr[Address],
       path: Seq[Expr[Layout.Path]],
       typeInfo: TypeInfo
   )(using q: Quotes, t: Type[A], u: Type[Layout.Path]): Expr[Any] =
      import quotes.reflect.*
      typeInfo match
         case PrimitiveInfo(name, '[a]) =>
            val updatedPath = Expr.ofSeq(path :+ '{
               Layout.Path.groupElement(${ Expr(name) })
            })
            val info = Expr.summonOrError[NativeInfo[A]]
            '{
               ${ Expr.summonOrError[Reader[a]] }.from(
                 $memorySegmentExpr,
                 $info.layout.byteOffset($updatedPath*)
               )
            }
         case ProductInfo(name, members, '[a]) =>
            val updatedPath =
               if name.isEmpty then path
               else path :+ '{ Layout.Path.groupElement(${ Expr(name) }) }
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
            deserializerFromTypeInfo[A](
              memorySegmentExpr,
              path,
              PrimitiveInfo(name, t)
            )

   private def serializerFromTypeInfo(
       a: Expr[?],
       memoryAddress: Expr[Address],
       offset: Expr[Long],
       layout: Expr[Layout],
       path: Seq[Expr[Layout.Path]],
       typeInfo: TypeInfo
   )(using Quotes, Type[Layout.Path]): Expr[Unit] =
      import quotes.reflect.*
      typeInfo match
         case PrimitiveInfo(name, '[a]) =>
            val to = Expr.summonOrError[Writer[a]]
            val pathExpr = Expr.ofSeq(path)
            '{
               $to.into(
                 ${ a.asExprOf[a] },
                 $memoryAddress,
                 ${layout}.byteOffset($pathExpr*) + $offset
               )
            }
         case ProductInfo(name, members, '[a]) =>
            val aTerm = a.asTerm
            val aMembers =
               TypeRepr.of[a].typeSymbol.caseFields.map(s => s.name -> s).toMap
            val memberSelect = members.map { m =>
               val updatedPath = path :+ '{
                  Layout.Path.groupElement(${ Expr(m.name) })
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
            Expr.block(memberSelect.toList, '{})

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
