package io.gitlab.mhammons.slinc.components

import ffi.{Layout, Linker, Address, CInt, CFloat, CDouble, CLong, CPointer, CShort, CChar}

type Informee[A, B] = NativeInfo[A] ?=> B
def infoOf[A]: Informee[A, NativeInfo[A]] = summon[NativeInfo[A]]
def layoutOf[A]: Informee[A, Layout] = infoOf[A].layout
trait NativeInfo[A]:
   val layout: Layout

object NativeInfo:

   def apply[A](using NativeInfo[A]) = summon[NativeInfo[A]]
   given NativeInfo[Int] with
      val layout = CInt
   given NativeInfo[Float] with
      val layout = CFloat

   given NativeInfo[Double] with
      val layout = CDouble

   given NativeInfo[Long] with
      val layout = CLong

   given NativeInfo[String] with
      val layout = CPointer

   given NativeInfo[Short] with
      val layout = CShort

   given NativeInfo[Boolean] with
      val layout = CChar

   given NativeInfo[Byte] with
      val layout = CChar

   given NativeInfo[Char] with
      val layout = CShort

   given [A]: NativeInfo[Array[A]] =
      summon[NativeInfo[String]].asInstanceOf[NativeInfo[Array[A]]]

   given [A](using Fn[A]): NativeInfo[A] with
      val layout = CPointer
