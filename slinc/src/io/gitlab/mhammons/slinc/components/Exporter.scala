package io.gitlab.mhammons.slinc.components

import ffi.{Address, ForeignSymbol, Scope, Descriptor, Linker, Allocator, CInt, CLong, CFloat, CShort, CChar, CDouble}
import io.gitlab.mhammons.slinc.components.{Linker => CLinker}
import scala.compiletime.erasedValue
import io.gitlab.mhammons.polymorphics.VoidHelper
import java.lang.invoke.{MethodType => MT, MethodHandles}
import scala.quoted.*
import scala.util.chaining.*
import scala.reflect.ClassTag
import java.lang.invoke.MethodHandle

/** Indicates how to turn JVM types into [[io.gitlab.mhammons.slincffi.FFI#Address]]
  * @tparam A
  *   The type to be turned into a MemoryAddresses
  */
trait Exporter[A]:
   def exportValue(a: A)(using Scope, Allocator): Address

type Exportee[A, B] = Exporter[A] ?=> B

def exportValue[A](a: A): Scopee[Allocatee[Exportee[A, Address]]] =
   summon[Exporter[A]].exportValue(a)

object Exporter:
   private inline def allocatingSerialize[A](
       a: A
   ): Allocatee[Informee[A, Writee[A, Address]]] =

      val address = allocate[A].address
      write(a, address, 0)
      address

   def derive[A]: Informee[A, Writee[A, Exporter[A]]] =
      new Exporter[A]:
         def exportValue(a: A)(using Scope, Allocator) = allocatingSerialize(a)

   given Exporter[Int] with 
      def exportValue(a: Int)(using Scope, Allocator) = segAlloc.allocate(CInt, a)
   given Exporter[Long] with 
      def exportValue(a: Long)(using Scope, Allocator) = segAlloc.allocate(CLong, a)
   given Exporter[Float] with 
      def exportValue(a: Float)(using Scope, Allocator) = segAlloc.allocate(CFloat, a)

   given Exporter[Double] with 
      def exportValue(a: Double)(using Scope, Allocator) = segAlloc.allocate(CDouble, a)

   given Exporter[Short] with 
      def exportValue(a: Short)(using Scope, Allocator) = segAlloc.allocate(CShort, a)

   given Exporter[Boolean] with
      def exportValue(a: Boolean)(using Scope, Allocator) = segAlloc.allocate(CChar, if a then 1.toByte else 0.toByte)

   // given Exporter[Char] with
   //    def exportValue(a: Char) = allocatingSerialize(a)

   given Exporter[Byte] with 
      def exportValue(a: Byte)(using Scope, Allocator) = segAlloc.allocate(CChar, a)


   given [A](using Writer[Array[A]], NativeInfo[A]): Exporter[Array[A]] with
      def exportValue(a: Array[A])(using Scope, Allocator) =
         val address =
            segAlloc.allocate(layoutOf[A].byteSize * a.length).address
         write[Array[A]](a, address, 0)
         address

   
   inline given fnExporter[A](using Fn[A]): Exporter[A] = ${
      fnExporterImpl[A]
   }

   private val paramNames =
      LazyList.iterate('a', 24)(c => (c.toInt + 1).toChar).map(_.toString)

   private def fnExporterImpl[A](using Quotes, Type[A]): Expr[Exporter[A]] =
      import quotes.reflect.*

      '{
         given Fn[A] = ${ Expr.summonOrError[Fn[A]] }
         new Exporter[A]:
            val typeName = ${ Expr(TypeRepr.of[A].typeSymbol.name) }

            val functionDescriptor =
               MethodHandleMacros.functionDescriptorForFn[A]

            val methodType = 
               Linker.downcallType(functionDescriptor)

            def exportValue(a: A)(using Scope, Allocator): Address = ${
               val aTerm = 'a.asTerm
               val (nTypeRepr, inputTypes, retType) = TypeRepr.of[A] match
                  case AppliedType(oTypeRepr, args) =>
                     val types = args.map(_.asType)
                     (
                       oTypeRepr.appliedTo(args.map(_ => TypeRepr.of[Any])),
                       types.init,
                       types.last
                     )

               val wrappedLambda = nTypeRepr.asType.pipe {
                  case '[nLambdaType] =>
                     Lambda(
                       Symbol.spliceOwner,
                       MethodType(paramNames.take(inputTypes.size).toList)(
                         _ => inputTypes.map(_ => TypeRepr.of[Any]),
                         _ => TypeRepr.of[Any]
                       ),
                       (_, params) =>
                          params
                             .map(_.asExpr)
                             .zip(inputTypes)
                             .map { case (p, '[a]) =>
                                val immigrator =
                                   Expr.summonOrError[Immigrator[a]]
                                '{ $immigrator($p) }.asTerm
                             }
                             .pipe(terms =>
                                retType.pipe { case '[r] =>
                                   val symbol = TypeRepr
                                      .of[A]
                                      .typeSymbol
                                      .declaredMethod("apply")
                                      .head
                                   val selected = Select(aTerm, symbol)
                                   val expr = Apply(selected, terms).asExprOf[r]
                                   val emigrator =
                                      Expr.summonOrError[Emigrator[r]]
                                   '{ $emigrator($expr) }.asTerm
                                }
                             )
                     ).asExprOf[nLambdaType]
               }

               val classRepr = nTypeRepr.asType.pipe { case '[nLambdaType] =>
                  val classTag = Expr.summonOrError[ClassTag[nLambdaType]]
                  '{ $classTag.runtimeClass }
               }
               '{
                  val lambdaMh: MethodHandle = MethodHandles.lookup
                     .findVirtual(
                       $classRepr,
                       "apply",
                       MT.genericMethodType(${ Expr(inputTypes.size) })
                     )
                     .bindTo($wrappedLambda)
                     .asType(methodType)

                  CLinker.linker.upcallStub(
                    lambdaMh,
                    functionDescriptor,
                    currentScope
                  ).address
               }
            }
      }
