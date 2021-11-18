package io.gitlab.mhammons.slinc

import jdk.incubator.foreign.MemorySegment
import io.gitlab.mhammons.polymorphics.VarHandleHandler
import jdk.incubator.foreign.GroupLayout
import jdk.incubator.foreign.MemoryLayout
import scala.quoted.*
import scala.util.chaining.*
import java.lang.invoke.VarHandle
import io.gitlab.mhammons.slinc.components.Ptr
import components.{StructInfo, StructStub, PrimitiveInfo, NamedVarhandle}
import io.gitlab.mhammons.slinc.components.MemLayout
import io.gitlab.mhammons.slinc.components.StructLayout


class Struct(vals: Map[String, Any], memorySegment: MemorySegment)
    extends Selectable:
   def selectDynamic(name: String) = vals(name)

   private[slinc] val $mem = memorySegment

object Struct:
   extension [S <: Struct](s: S) inline def `unary_~` = Ptr[S](s)

object StructMacros:
   def getStructInfo[A: Type](using Quotes): StructInfo =
      import quotes.reflect.*
      TypeRepr.of[A].dealias match
         case Refinement(ancestor, name, typ) =>
            val typType = typ.asType

            val thisType: PrimitiveInfo | StructStub = typType match
               case '[Struct] =>
                  StructStub(name, typType)

               case '[a] =>
                  PrimitiveInfo(name, typType)
            ancestor.asType.pipe { case '[a] =>
               getStructInfo[a].pipe(res =>
                  res.copy(members = res.members :+ thisType)
               )
            }

         case repr if repr =:= TypeRepr.of[Struct] =>
            StructInfo(None, Seq.empty)

         case t =>
            report.errorAndAbort(
              s"Cannot extract refinement data for non-struct type ${t.show(using Printer.TypeReprCode)}"
            )
   end getStructInfo

   inline def structFromMemSegment[A](memSegment: MemorySegment): A = ${
      structFromMemSegmentImpl[A]('memSegment)
   }

   private def structFromMemSegmentImpl[A: Type](
       memSegmentExpr: Expr[MemorySegment]
   )(using Quotes) =
      import quotes.reflect.*

      def fn(parentLayout: Expr[StructLayout]): Expr[Seq[(String, ?)]] =
         getStructInfo[A].members
            .flatMap {
               case StructStub(name, '[a]) =>
                  Seq('{
                     (
                       ${ Expr(name) },
                       structFromMemSegment[a](
                         $parentLayout
                            .subsegmntOf(${ Expr(name) }, $memSegmentExpr)
                       )
                     )
                  })
               case _ => Nil
            }
            .pipe(Expr.ofSeq)

      val nc = Expr.summon[NativeCache].getOrElse(missingNativeCache)

      '{
         val msgmnt = $memSegmentExpr
         val nativeCache = $nc

         val layout = nativeCache.layout[A]

         val varHandles = nativeCache
            .varHandles[A]
            .map(nvh => (nvh.name, Member[Any](msgmnt, nvh.varhandle)))

         val structs = ${ Expr.betaReduce(fn('layout)) }

         Struct(varHandles.toMap ++ structs.toMap, msgmnt).asInstanceOf[A]
      }.tap(e => report.info(e.show))

   inline def genVarHandles[A] = ${
      genVarHandlesImpl[A]
   }

   private def genVarHandlesImpl[A: Type](using q: Quotes) =
      import quotes.reflect.report
      import TransformMacros.{type2MethodTypeArg, type2MemLayout}
      val structInfo = getStructInfo[A]
      val nCache = Expr.summon[NativeCache].getOrElse(missingNativeCache)

      { (layoutExpr: Expr[MemLayout]) =>
         {
            structInfo.members
               .flatMap {

                  case PrimitiveInfo(name, '[a]) =>
                     val nameExp = Expr(name)
                     List(
                       '{
                          NamedVarhandle(
                            $nameExp,
                            $layoutExpr.varHandle(
                              ${ type2MethodTypeArg[a] },
                              MemoryLayout.PathElement.groupElement($nameExp)
                            )
                          )
                       }
                     )

                  case _ => Nil
               }
               .pipe(Expr.ofSeq)
         }
      }
         .pipe(lambdaList =>
            Expr.betaReduce('{
               val layout = $nCache.layout[A]
               ${ lambdaList('layout) }

            })
         )
         .tap(_.show.tap(report.info))
