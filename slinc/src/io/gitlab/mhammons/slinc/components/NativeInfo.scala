package io.gitlab.mhammons.slinc.components

import jdk.incubator.foreign.{MemoryLayout, CLinker, MemoryAddress},
CLinker.{C_INT, C_FLOAT, C_DOUBLE, C_LONG, C_POINTER, C_SHORT, C_CHAR}

type Informee[A, B] = NativeInfo[A] ?=> B
def infoOf[A]: Informee[A, NativeInfo[A]] = summon[NativeInfo[A]]
trait NativeInfo[A]:
   val layout: MemoryLayout
   val carrierType: Class[?]

object NativeInfo:

   def apply[A](using NativeInfo[A]) = summon[NativeInfo[A]]
   given NativeInfo[Int] with
      val layout = C_INT
      val carrierType = classOf[Int]

   given NativeInfo[Float] with
      val layout = C_FLOAT
      val carrierType = classOf[Float]

   given NativeInfo[Double] with
      val layout = C_DOUBLE
      val carrierType = classOf[Double]

   given NativeInfo[Long] with
      val layout = C_LONG
      val carrierType = classOf[Long]

   given NativeInfo[String] with
      val layout = C_POINTER
      val carrierType = classOf[MemoryAddress]

   given NativeInfo[Short] with
      val layout = C_SHORT
      val carrierType = classOf[Short]

   given NativeInfo[Boolean] with
      val layout = C_CHAR
      val carrierType = classOf[Boolean]

   given NativeInfo[Byte] with
      val layout = C_CHAR
      val carrierType = classOf[Byte]

   given NativeInfo[Char] with
      val layout = C_CHAR
      val carrierType = classOf[Char]

   given [A]: NativeInfo[Array[A]] =
      summon[NativeInfo[String]].asInstanceOf[NativeInfo[Array[A]]]
