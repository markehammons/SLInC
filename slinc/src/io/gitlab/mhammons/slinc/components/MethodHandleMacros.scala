package io.gitlab.mhammons.slinc.components

import scala.quoted.*
import scala.util.chaining.*
import ffi.{
   Descriptor,
   Allocator,
   Address,
   Layout,
   Lookup,
   ForeignSymbol
}
import io.gitlab.mhammons.polymorphics.{MethodHandleHandler, VoidHelper}
import java.lang.invoke.MethodHandle
import java.lang.invoke.MethodType

import cats.data.Validated
import cats.implicits.*

private[slinc] object MethodHandleMacros:
   inline def functionDescriptorForFn[A](using Fn[A]): Descriptor =
      ${
         functionDescriptorForFnImpl[A]
      }

   private def functionDescriptorForFnImpl[A](using q: Quotes)(using
       Type[A]
   ): Expr[Descriptor] =
      import quotes.reflect.*

      TypeRepr.of[A] match
         case AppliedType(_, args) =>
            val types = args.map(_.asType)
            val inputs = types.init.map { case '[a] =>
               '{ ${ Expr.summonOrError[NativeInfo[a]] }.layout }
            }
            val retType = types.last.pipe { case '[r] =>
               Expr.summon[NativeInfo[r]].map(e => '{ $e.layout })
            }

            functionDescriptor(retType, inputs)(Nil)

   private def functionDescriptor(
       returnLayout: Option[Expr[Layout]],
       paramLayouts: List[Expr[Layout]]
   )(
       vParamLayouts: List[Expr[Layout]]
   )(using Quotes): Expr[Descriptor] =
      returnLayout
         .map { r =>
            '{
               Descriptor
                  .of($r, ${ Varargs(paramLayouts) }*)
                  .asVariadic(${ Varargs(vParamLayouts) }*)
            }
         }
         .getOrElse('{
            Descriptor
               .ofVoid(${ Varargs(paramLayouts) }*)
               .asVariadic(${ Varargs(vParamLayouts) }*)
         })

   private def paramsToLayoutAndCarrier(params: List[Expr[Any]])(using
       Quotes
   ): List[Expr[Layout]] =
      params
         .map(_.widen)
         .flatMap { case '{ $z: a } =>
            val a = Expr.summonOrError[NativeInfo[a]]
            Seq('{ $a.layout })
         }

   private def dc2(
       addr: Expr[ForeignSymbol],
       paramInfo: List[Expr[NativeInfo[?]]],
       vParamInfo: List[Expr[NativeInfo[?]]],
       returnInfo: Option[Expr[NativeInfo[?]]]
   )(using Quotes): Expr[MethodHandle] =

      val layouts =
         paramInfo.map(i => '{ $i.layout })

      val vLayouts =
         vParamInfo.map(i => '{ $i.layout })
      val functionD =
         functionDescriptor(returnInfo.map(i => '{ $i.layout }), layouts)(
           vLayouts
         )

      '{
         Linker.linker.downcallHandle(
           $addr,
           $functionD,
           localAllocator
         )
      }


   def call2(mh: Expr[MethodHandle], ps: List[Expr[Any]])(using
       Quotes
   ): Expr[Any] =
      import quotes.reflect.*
      Apply(
        Ident(
          TermRef(TypeRepr.of[MethodHandleHandler.type], s"call${ps.size}")
        ),
        mh.asTerm +: ps.map(_.asTerm)
      ).asExprOf[Any]

   def vdAndRefFromExpr[A](expr: Expr[A], name: String)(using
       q: Quotes,
       t: Type[A]
   ): (q.reflect.ValDef, q.reflect.Ref) =
      import quotes.reflect.*
      val sym = Symbol.newVal(
        Symbol.spliceOwner,
        name,
        TypeRepr.of[A],
        Flags.EmptyFlags,
        Symbol.noSymbol
      )

      val vd = ValDef(sym, Some(expr.asTerm.changeOwner(sym)))
      val ref = Ref(sym)
      vd -> ref

   def wrappedMH(
       memoryAddress: Expr[ForeignSymbol],
       inputNames: List[Option[String]],
       inputTypes: List[Type[?]],
       returnType: Type[?],
       inputVariadicTypes: List[Type[?]] = Nil
   )(using q: Quotes): Validated[List[String], Expr[Any]] =
      import quotes.reflect.{MethodType => MT, *}
      case class MHState(
          inputNIVds: List[ValDef],
          inputNIRefs: List[Ref],
          inputEmiVds: List[ValDef],
          inputEmiRefs: List[Ref],
          retNIVd: Option[ValDef],
          retNIRef: Option[Ref],
          retImmiVd: ValDef,
          retImmiRef: Ref,
          mhVd: ValDef,
          mhRef: Ref
      )

      val resolvedInputNames = inputNames.map(
        _.map(s => s"parameter $s").getOrElse("variadic argument")
      )
      def summonOrInvalid[A](inputName: String)(using Type[A]) = Validated.fromOption(
        Expr.summon[A],
        List(s"Could not summon ${Type.show[A]} for $inputName. Is it defined?")
      )

      val paramNames =
         LazyList.iterate('a')(a => (a + 1).toChar).map(_.toString)

      val inputNIs =
         resolvedInputNames
            .zip(inputTypes)
            .zip(paramNames.map(s => s"ni$s"))
            .traverse { case ((inputName, '[a]), name) =>
               summonOrInvalid[NativeInfo[a]](inputName)
                  .map(vdAndRefFromExpr(_, name))
            }
            .map(_.unzip)

      val vInputNIs =
         inputVariadicTypes.zipWithIndex
            .zip(paramNames.map(n => s"vNi$n"))
            .traverse { case (('[a], idx), name) =>
               summonOrInvalid[NativeInfo[a]](idx.toString).map(
                 vdAndRefFromExpr(_, name)
               )
            }
            .map(_.unzip)

      val inputEmis = resolvedInputNames
         .zip(inputTypes)
         .zip(paramNames.map(s => s"emi$s"))
         .traverse { case ((inputName, '[a]), name) =>
            summonOrInvalid[Emigrator[a]](inputName)
               .map(vdAndRefFromExpr(_, name))
         }
         .map(_.unzip)

      val variadicEmis = inputVariadicTypes.zipWithIndex
         .zip(paramNames.map(s => s"vEmi$s"))
         .traverse { case (('[a], idx), name) =>
            summonOrInvalid[Emigrator[a]](idx.toString).map(
              vdAndRefFromExpr(_, name)
            )
         }
         .map(_.unzip)

      val retNI = returnType match
         case '[Unit] =>
            Validated.valid((Option.empty[ValDef], Option.empty[Ref]))
         case '[r] =>
            summonOrInvalid[NativeInfo[r]]("the binding return").map(
              vdAndRefFromExpr(_, "retNI").pipe(t => (Some(t._1), Some(t._2)))
            )

      val retImmi = returnType.match { case '[r] =>
         summonOrInvalid[Immigrator[r]]("the binding return").map(vdAndRefFromExpr(_, "retImmi"))
      }

      val state = inputNIs.andThen((inputVds, inputRefs) =>
         vInputNIs.andThen((vInputVds, vInputRefs) => 
            variadicEmis.andThen((vEmiVDs, vEmiRefs) => 
         inputEmis.andThen((emiVDs, emiRefs) =>
            retNI.andThen((retNIVd, retNIRef) =>
               retImmi.map { (immiVd, immiRef) =>
                  val (mhVd, mhRef) = vdAndRefFromExpr(
                    dc2(
                      memoryAddress.asExprOf[ForeignSymbol],
                      inputRefs.map(_.asExprOf[NativeInfo[?]]),
                      vInputRefs.map(_.asExprOf[NativeInfo[?]]),
                      retNIRef.map(_.asExprOf[NativeInfo[?]])
                    ),
                    "mh"
                  )
                  MHState(
                    inputVds ++ vInputVds,
                    inputRefs ++ vInputRefs,
                    emiVDs ++ vEmiVDs,
                    emiRefs ++ vEmiRefs,
                    retNIVd,
                    retNIRef,
                    immiVd,
                    immiRef,
                    mhVd,
                    mhRef
                  )
               }
            )
         )
      )))

      def lmb(s: MHState) =
         Lambda(
           Symbol.spliceOwner,
           MT(paramNames.take((inputTypes ++ inputVariadicTypes).size).toList)(
             _ => (inputTypes ++ inputVariadicTypes).map { case '[a] => TypeRepr.of[a] },
             _ => returnType match { case '[r] => TypeRepr.of[r] }
           ),
           (owner, trees) =>
              returnType match {
                 case '[r] =>
                    val retImmi = s.retImmiRef.asExprOf[Immigrator[r]]
                    val callExpr = '{
                       given Allocator = localAllocator
                       try ${
                          call2(
                            s.mhRef.asExprOf[MethodHandle],
                            (inputTypes ++ inputVariadicTypes).zip(trees.zip(s.inputEmiRefs)).map {
                               case ('[a], (t, emi)) =>
                                  '{
                                     ${ emi.asExprOf[Emigrator[a]] }(${
                                        t.asExprOf[a]
                                     })
                                  }
                            }
                          )
                       } finally reset()
                    }
                    '{
                       $retImmi($callExpr)
                    }.asTerm.changeOwner(owner)
              }
         )

      state.map(s =>
         Block(
           s.inputEmiVds ++ s.inputNIVds ++ s.retNIVd.toList ++
              List(
                s.retImmiVd,
                s.mhVd
              ),
           lmb(s)
         ).asExpr//.tap(_.show.tap(report.errorAndAbort))
      )
   end wrappedMH

   def wrappedMHFromDefDef(using
       q: Quotes
   )(
       s: q.reflect.Symbol,
       address: Expr[ForeignSymbol]
   ): Validated[List[String], Expr[Any]] =
      import quotes.reflect.{MethodType => MT, *}
      val paramNames =
         LazyList.iterate('a')(a => (a + 1).toChar).map(_.toString)

      val ((inputNames, inputTypes), returnType) = s.tree match
         case DefDef(name, parameters, returnType, _) =>
            parameters
               .flatMap(_.params.collect { case ValDef(a, b, _) =>
                  Some(a) -> b.tpe.asType
               })
               .unzip -> returnType.tpe.asType

      wrappedMH(address, inputNames, inputTypes, returnType)

   end wrappedMHFromDefDef
end MethodHandleMacros
